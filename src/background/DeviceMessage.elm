module DeviceMessage where

-- Elm standard library
import Maybe
import List (..)
import Bitwise(and)

-- local source
import CommonState (..)
import CommonState as Common
import BackgroundState (..)
import DevicePacket (..)
import DeviceFlash (..)
import Util (..)
import Byte (..)

type alias FromDeviceMessage = { setHidConnected : Maybe Bool
                               , receiveCommand  : Maybe (List Int)
                               , appendToLog     : Maybe String
                               }

type alias ToDeviceMessage   = { connect     : Maybe ()
                               , sendCommand : Maybe (List Int)
                               }


decodeStatus : Int -> BackgroundAction
decodeStatus i = case i `and` 0x7 of
    0x0 -> CommonAction (SetDeviceStatus NoCard)
    0x1 -> CommonAction (SetDeviceStatus Locked)
    0x3 -> CommonAction (SetDeviceStatus Locked)
    0x5 -> CommonAction (SetDeviceStatus Unlocked)
    0x7 -> NoOp -- used to get past dropRepeats
    _   -> appendToLog' "Error: Received invalid status from device"

sendCommand : OutgoingPacket -> ToDeviceMessage
sendCommand p = {emptyToDeviceMessage | sendCommand <- Just (toInts p)}

connect : ToDeviceMessage
connect = {emptyToDeviceMessage | connect <- Just ()}

emptyToDeviceMessage = {connect = Nothing, sendCommand = Nothing}

decode : FromDeviceMessage -> List BackgroundAction
decode message =
    let decode' {setHidConnected, receiveCommand, appendToLog} =
        Maybe.oneOf
            [ Maybe.map (CommonAction << Common.AppendToLog) appendToLog
            , Maybe.map SetHidConnected setHidConnected
            , Maybe.map (fromResult << fromInts) receiveCommand
            ]
    in case Maybe.withDefault NoOp (decode' message) of
        NoOp -> []
        a -> [SetWaitingForDevice False, a]

encode : BackgroundState -> (ToDeviceMessage, List BackgroundAction)
encode s =
    let e = emptyToDeviceMessage
        memManageNeedsToSend = case s.memoryManage of
            MemManageRead    _ _ -> True
            MemManageReadFav _   -> True
            MemManageWrite   _   -> True
            MemManageRequested   -> True
            MemManageEnd         -> True
            _                    -> False
        extNeedsToSend' = extNeedsToSend s.extRequest && not s.waitingForDevice
                        && s.common.deviceStatus == Unlocked
    in if | not s.deviceConnected -> (connect, [])
          | extNeedsToSend' ->
              ({e | sendCommand <-
                    Maybe.map toInts
                        (extRequestToPacket s.currentContext s.extRequest)}
              , [ Maybe.withDefault NoOp
                    (Maybe.map
                        (CommonAction << AppendToLog)
                        (outgoingExtRequestToLog s.extRequest))
                , SetWaitingForDevice True
                ]
              )
          | s.deviceVersion == Nothing
            && s.common.deviceStatus == Unlocked && not s.waitingForDevice
                -> sendCommand' OutgoingGetVersion []
          | mediaImportActive s -> case s.mediaImport of
                MediaImportStart ps ->
                    sendCommand'
                        OutgoingImportMediaStart
                        [SetMediaImport (MediaImportStartWaiting ps)]
                MediaImport (p::ps) ->
                    sendCommand' p
                        [SetMediaImport (MediaImportWaiting (p::ps))]
                MediaImport [] ->
                    sendCommand'
                        OutgoingImportMediaEnd
                        [SetMediaImport (MediaImportWaiting [])]
                _ -> (e, [])
          | memManageNeedsToSend -> case s.memoryManage of
              MemManageEnd ->    sendCommand'
                                        OutgoingMemManageModeEnd
                                        [SetMemManage NotManaging]
              MemManageRequested -> sendCommand'
                                        OutgoingMemManageModeStart
                                        [SetMemManage MemManageWaiting, CommonAction (SetDeviceStatus ManageMode)]
              MemManageRead (p,addr,nPAddr) ba -> case p of
                  EmptyParentNode ->
                      if addr == null then
                        sendCommand'
                            OutgoingGetStartingParent
                            [SetMemManage (MemManageReadWaiting (p,null,null) ba)]
                      else
                        sendCommand'
                            (OutgoingReadFlashNode addr)
                            [SetMemManage (MemManageReadWaiting (p,addr,nPAddr) ba)]
                  ParentNode d ->
                        if  | addr /= null ->
                                sendCommand'
                                    (OutgoingReadFlashNode addr)
                                    [SetMemManage (MemManageReadWaiting (p,addr,nPAddr) ba)]
                            | otherwise ->
                                sendCommand'
                                    (OutgoingGetFavorite 0)
                                    [SetMemManage (MemManageReadFavWaiting (linkNextParentsReturnFirst p,[]))]
              MemManageReadFav (p,favs) ->
                        sendCommand'
                            (OutgoingGetFavorite (length favs))
                            [SetMemManage (MemManageReadFavWaiting (p,favs))]
              MemManageWrite (p::ps) ->
                        sendCommand'
                            p
                            [SetMemManage (MemManageWriteWaiting (p::ps))]
              MemManageWrite [] ->
                        sendCommand'
                            OutgoingGetStartingParent
                            [SetMemManage (MemManageReadWaiting (EmptyParentNode,null,null) [])]
          | not (mediaImportActive s) && not (memoryManageBusy s.memoryManage) ->
              ({ e | sendCommand <- Just (toInts OutgoingGetStatus)}
              , [])
          | otherwise -> (e,[])

sendCommand' : OutgoingPacket
            -> (List BackgroundAction)
            -> (ToDeviceMessage, List BackgroundAction)
sendCommand' p a = (sendCommand p, SetWaitingForDevice True::a)

extNeedsToSend r = case r of
    ExtNeedsNewContext _       -> True
    ExtWantsCredentials _      -> True
    ExtNeedsLogin _            -> True
    ExtNeedsPassword _         -> True
    ExtWantsToWrite _          -> True
    ExtNeedsToWritePassword _  -> True
    _                          -> False

extRequestToPacket : String -> ExtensionRequest -> Maybe OutgoingPacket
extRequestToPacket cc extRequest =
    case extRequest of
        ExtNeedsNewContext {context, login, password} ->
            Just (OutgoingAddContext context)
        ExtWantsCredentials {context} ->
            Just (OutgoingSetContext context)
        ExtNeedsLogin {context} ->
            if cc == context then Just OutgoingGetLogin
            else Just (OutgoingSetContext context)
        ExtNeedsPassword {context, login} ->
            if cc == context then Just OutgoingGetPassword
            else Just (OutgoingSetContext context)
        ExtWantsToWrite {context, login, password} ->
            if cc == context then Just (OutgoingSetLogin login)
            else Just (OutgoingSetContext context)
        ExtNeedsToWritePassword {context, password} ->
            if cc == context then Just (OutgoingSetPassword password)
            else Just (OutgoingSetContext context)
        _ -> Nothing

