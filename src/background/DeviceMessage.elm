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

sendCommand : SendPacket -> ToDeviceMessage
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
                            SendImportMediaStart
                            [SetMediaImport (MediaImportStartWaiting ps)]
                    MediaImport (p::ps) ->
                        sendCommand' p
                            [SetMediaImport (MediaImportWaiting (p::ps))]
                    MediaImport [] ->
                        sendCommand'
                            SendImportMediaEnd
                            [SetMediaImport (MediaImportWaiting [])]
                    _ -> (e, [])
          | s.deviceVersion == Nothing
            && s.common.deviceStatus == Unlocked
                -> sendCommand' SendGetVersion []
          | s.memoryManage == MemManageRequested ->
              sendCommand'
                 SendMemManageModeStart
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

sendCommand' : SendPacket
            -> (List BackgroundAction)
            -> (ToDeviceMessage, List BackgroundAction)
sendCommand' p a = (sendCommand p, SetWaitingForDevice True::a)

toPacket : String -> ExtensionRequest -> Maybe SendPacket
toPacket cc extRequest =
    case extRequest of
        ExtNeedsNewContext {context, login, password} ->
            Just (SendAddContext context)
        ExtWantsCredentials {context} ->
            if cc == context then Just SendGetLogin
            else Just (SendSetContext context)
        ExtNeedsPassword {context, login} ->
            if cc == context then Just SendGetPassword
            else Just (SendSetContext context)
        ExtWantsToWrite {context, login, password} ->
            if cc == context then Just (SendSetLogin login)
            else Just (SendSetContext context)
        ExtNeedsToWritePassword {context, password} ->
            if cc == context then Just (SendSetPassword password)
            else Just (SendSetContext context)
        _ -> Nothing

