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
    in if | not s.hidConnected -> ({e | connect <- Just ()}, NoOp)
          -- this is take caro of with keep-alive
          | s.common.connected /= Common.Connected -> (e, NoOp)
          | s.deviceVersion == Nothing ->
              ({e | sendCommand <- Just (toInts AppGetVersion)}, NoOp)
          | s.extRequest /= NoRequest ->
              ({e | sendCommand <- Maybe.map toInts (toPacket s)}
              , Maybe.withDefault NoOp
                    (Maybe.map
                        (CommonAction << AppendToLog)
                        (extensionRequestToLog s.extRequest)))
          | otherwise -> (e,NoOp)

toPacket : BackgroundState -> Maybe AppPacket
toPacket s =
    let cc = s.currentContext
    in case s.extRequest of
        ExtNeedsNewContext {context, login, password} ->
            Just (AppAddContext context)
        ExtWantsCredentials {context} ->
            if cc == context then Just AppGetLogin
            else Just (AppSetContext context)
        ExtNeedsLogin {context} ->
            if cc == context then Just AppGetLogin
            else Just (AppSetContext context)
        ExtNeedsPassword {context, login} ->
            if cc == context then Just AppGetPassword
            else Just (AppSetContext context)
        ExtWantsToWrite {context, login, password} ->
            if cc == context then Just (AppSetLogin login)
            else Just (AppSetContext context)
        ExtNeedsToWriteLogin {context, login, password} ->
            if cc == context then Just (AppSetLogin login)
            else Just (AppSetContext context)
        ExtNeedsToWritePassword {context, password} ->
            if cc == context then Just (AppSetPassword password)
            else Just (AppSetContext context)
        _ -> Nothing

