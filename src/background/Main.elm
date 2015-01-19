module Background where

-- Elm standard library
import Signal (..)

-- local source
import ToGuiMessage
import ToGuiMessage (ToGuiMessage)
import FromGuiMessage
import FromGuiMessage (FromGuiMessage)
import CommonState (CommonAction)
import DeviceMessage (..)
import BackgroundState (..)

port fromGUI : Signal FromGuiMessage

port toGUI : Signal ToGuiMessage
port toGUI = (ToGuiMessage.encode << .common) <~ state

port fromDevice : Signal FromDeviceMessage

port toDevice : Signal ToDeviceMessage
port toDevice = constant {connect = Nothing, sendCommand = Nothing}

state : Signal BackgroundState
state =
    foldp update default
        <| merge (decode <~ fromDevice)
        <| CommonAction <~ (FromGuiMessage.decode <~ fromGUI)
