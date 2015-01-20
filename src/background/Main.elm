module Background where

-- Elm standard library
import Signal (..)

-- local source
import ToGuiMessage
import ToGuiMessage (ToGuiMessage)
import FromGuiMessage
import FromGuiMessage (FromGuiMessage)
import CommonState (CommonAction)
import DeviceMessage (FromDeviceMessage, ToDeviceMessage)
import DeviceMessage
import BackgroundState (..)
import ExtensionMessage (FromExtensionMessage, ToExtensionMessage)
import ExtensionMessage

port fromGUI : Signal FromGuiMessage

port toGUI : Signal ToGuiMessage
port toGUI = (ToGuiMessage.encode << .common) <~ state

port fromDevice : Signal FromDeviceMessage

port toDevice : Signal ToDeviceMessage
port toDevice = constant {connect = Nothing, sendCommand = Nothing}

port fromExtension : Signal FromExtensionMessage

--port toExtension : Signal ToExtensionMessage
--port toExtension =

state : Signal BackgroundState
state =
    foldp update default
        <| mergeMany
            [ DeviceMessage.decode <~ fromDevice
            , CommonAction <~ (FromGuiMessage.decode <~ fromGUI)
            , ExtensionMessage.decode <~ fromExtension
            ]
