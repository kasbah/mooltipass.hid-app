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

sendCommand : AppPacket -> ToDeviceMessage
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
            , Maybe.map (fromPacket << fromInts) receiveCommand
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
                            AppImportMediaStart
                            [SetMediaImport (MediaImportStartWaiting ps)]
                    MediaImport (p::ps) ->
                        sendCommand' p
                            [SetMediaImport (MediaImportWaiting (p::ps))]
                    MediaImport [] ->
                        sendCommand'
                            AppImportMediaEnd
                            [SetMediaImport (MediaImportWaiting [])]
                    _ -> (e, [])
          | s.deviceVersion == Nothing && s.common.connected == Connected -> sendCommand' AppGetVersion []
          | s.extRequest /= NoRequest && s.common.connected == Connected ->
              ({e | sendCommand <-
                    Maybe.map toInts (toPacket s.currentContext s.extRequest)}
              , [ Maybe.withDefault NoOp
                    (Maybe.map
                        (CommonAction << AppendToLog)
                        ( extensionRequestToLog s.extRequest))
                , SetWaitingForDevice True
                ]
              )
          | otherwise -> (e, [])

sendCommand' : AppPacket
            -> (List BackgroundAction)
            -> (ToDeviceMessage, List BackgroundAction)
sendCommand' p a = (sendCommand p, SetWaitingForDevice True::a)

toPacket : String -> ExtensionRequest -> Maybe AppPacket
toPacket cc extRequest =
    case extRequest of
        ExtNeedsNewContext {context, login, password} ->
            Just (AppAddContext context)
        ExtWantsCredentials {context} ->
            if cc == context then Just AppGetLogin
            else Just (AppSetContext context)
        ExtNeedsPassword {context, login} ->
            if cc == context then Just AppGetPassword
            else Just (AppSetContext context)
        ExtWantsToWrite {context, login, password} ->
            if cc == context then Just (AppSetLogin login)
            else Just (AppSetContext context)
        _ -> Nothing

