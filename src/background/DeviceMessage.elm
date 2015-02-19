module DeviceMessage where

-- Elm standard library
import Maybe

-- local source
import CommonState (..)
import CommonState as Common
import BackgroundState (..)
import DevicePacket (..)
import Util (..)
import List ((::))

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
    in if | not s.deviceConnected -> (connect, [])
          | s.waitingForDevice -> (e, [])
          | mediaImportActive s ->
                case s.mediaImport of
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
          | s.memoryManage == MemManageRequested ->
              sendCommand'
                 OutgoingMemManageModeStart
                 [SetMemManage MemManageWaiting]
          | s.extRequest /= NoRequest && s.common.deviceStatus == Unlocked ->
              ({e | sendCommand <-
                    Maybe.map toInts (toPacket s.currentContext s.extRequest)}
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

toPacket : String -> ExtensionRequest -> Maybe OutgoingPacket
toPacket cc extRequest =
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

