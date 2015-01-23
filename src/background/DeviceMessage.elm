module DeviceMessage where

-- Elm standard library
import Maybe

-- local source
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
          | s.common.connected == Common.NotConnected ->
              ({e | sendCommand <- Just (toInts AppGetStatus)}, NoOp)
          | isJust s.extAwaitingData ->
              ({e | sendCommand <- Maybe.map toInts (toPacket s)},NoOp)
          | otherwise          -> (e,NoOp)

toPacket : BackgroundState -> Maybe AppPacket
toPacket s =
    let cc = s.currentContext
    in case s.extAwaitingData of
        Just (ExtNeedsLogin {context}) ->
            if cc == context then Just AppGetLogin
            else Just (AppSetContext context)
        Just (ExtNeedsPassword {context, login}) ->
            if cc == context then (Just AppGetPassword)
            else Just (AppSetContext context)
        Just (ExtUpdate {context, login, password}) ->
            if cc == context then Just (AppSetLogin login)
            else Just (AppSetContext context)
        Just (ExtUpdatePassword {context, password}) ->
            if cc == context then Just (AppSetPassword password)
            else Just (AppSetContext context)
        _ -> Nothing

