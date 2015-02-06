module DeviceMessage where

-- Elm standard library
import Maybe

-- local source
import CommonState (..)
import CommonState as Common
import BackgroundState (..)
import DevicePacket (..)
import Util (..)

type alias FromDeviceMessage = { setHidConnected : Maybe Bool
                               , receiveCommand  : Maybe (List Int)
                               , appendToLog     : Maybe String
                               }

type alias ToDeviceMessage   = { connect     : Maybe ()
                               , sendCommand : Maybe (List Int)
                               }

sendCommand : AppPacket -> ToDeviceMessage
sendCommand p = {emptyToDeviceMessage | sendCommand <- Just (toInts p)}

emptyToDeviceMessage = {connect = Nothing, sendCommand = Nothing}

decode : FromDeviceMessage -> BackgroundAction
decode message =
    let decode' {setHidConnected, receiveCommand, appendToLog} =
        Maybe.oneOf
            [ Maybe.map (CommonAction << Common.AppendToLog) appendToLog
            , Maybe.map SetHidConnected setHidConnected
            , Maybe.map (fromPacket << fromInts) receiveCommand
            ]
    in Maybe.withDefault NoOp (decode' message)

encode : BackgroundState -> (ToDeviceMessage, BackgroundAction)
encode s =
    let e = emptyToDeviceMessage
        activeImport = case s.mediaImport of
            NoMediaImport             -> False
            MediaImportError _        -> False
            MediaImportSuccess        -> False
            MediaImportStartWaiting _ -> False
            MediaImportWaiting _      -> False
            _ -> True
    in if | not s.deviceConnected -> ({e | connect <- Just ()}, NoOp)
          | activeImport ->
                case s.mediaImport of
                    MediaImportStart ps ->
                        ({e | sendCommand <- Just (toInts AppImportMediaStart)}
                        , SetMediaImport (MediaImportStartWaiting ps))
                    MediaImport (p::ps) ->
                        ({e | sendCommand <- Just (toInts p)}
                        , SetMediaImport (MediaImportWaiting (p::ps)))
                    MediaImport [] ->
                        ({e | sendCommand <- Just (toInts AppImportMediaEnd)}
                        , SetMediaImport (MediaImportWaiting []))
                    _ -> (e, NoOp)
          -- this is taken care of with keep-alive
          | s.common.connected /= Common.Connected -> (e, NoOp)
          | s.deviceVersion == Nothing ->
              ({e | sendCommand <- Just (toInts AppGetVersion)}, NoOp)
          | s.extRequest /= NoRequest ->
              ({e | sendCommand <-
                    Maybe.map toInts (toPacket s.currentContext s.extRequest)}
              , Maybe.withDefault NoOp
                    (Maybe.map
                        (CommonAction << AppendToLog)
                        (extensionRequestToLog s.extRequest)))
          | otherwise -> (e,NoOp)

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

