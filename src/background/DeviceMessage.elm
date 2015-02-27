module DeviceMessage where

-- Elm standard library
import Maybe
import List (..)

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
            MemManageRead    _ _   -> True
            MemManageReadFav _     -> True
            MemManageWrite   _     -> True
            MemManageRequested     -> True
            MemManageEnd           -> True
            _ -> False

    in if | not s.deviceConnected -> (connect, [])
          | s.waitingForDevice -> (e, [])
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
          | s.deviceVersion == Nothing
            && s.common.deviceStatus == Unlocked
                -> sendCommand' OutgoingGetVersion []
          | memManageNeedsToSend -> case s.memoryManage of
              MemManageEnd ->    sendCommand'
                                        OutgoingMemManageModeEnd
                                        [SetMemManage NotManaging]
              MemManageRequested -> sendCommand'
                                        OutgoingMemManageModeStart
                                        [SetMemManage MemManageWaiting]
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
          | s.extRequest /= NoRequest && s.common.deviceStatus == Unlocked ->
              ({e | sendCommand <-
                    Maybe.map toInts (extRequestToPacket s.currentContext s.extRequest)}
              , [ Maybe.withDefault NoOp
                    (Maybe.map
                        (CommonAction << AppendToLog)
                        (extensionRequestToLog s.extRequest))
                , SetWaitingForDevice True
                ]
              )
          | otherwise -> (e, [])

sendCommand' : OutgoingPacket
            -> (List BackgroundAction)
            -> (ToDeviceMessage, List BackgroundAction)
sendCommand' p a = (sendCommand p, SetWaitingForDevice True::a)
extRequestToPacket : String -> ExtensionRequest -> Maybe OutgoingPacket
extRequestToPacket cc extRequest =
    case extRequest of
        ExtNeedsNewContext {context, login, password} ->
            Just (OutgoingAddContext context)
        ExtWantsCredentials {context} ->
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

