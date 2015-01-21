module DeviceMessage where

-- Elm standard library
import Maybe

-- local source
import CommonState as Common
import BackgroundState (..)
import DevicePacket (..)

type alias FromDeviceMessage = { setHidConnected : Maybe Bool
                               , receiveCommand  : Maybe (List Int)
                               , appendToLog     : Maybe String
                               }

type alias ToDeviceMessage   = { connect     : Maybe ()
                               , sendCommand : Maybe (List Int)
                               }

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
          | otherwise          -> (e,NoOp)
