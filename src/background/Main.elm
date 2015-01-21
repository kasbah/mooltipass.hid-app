module Background where

-- Elm standard library
import Signal (..)

-- local source
import ToGuiMessage
import ToGuiMessage (ToGuiMessage)
import FromGuiMessage
import FromGuiMessage (FromGuiMessage)
import CommonState (CommonAction)
import DeviceMessage (FromDeviceMessage, ToDeviceMessage, emptyToDeviceMessage)
import DeviceMessage
import BackgroundState (..)
import ExtensionMessage (FromExtensionMessage, ToExtensionMessage, emptyToExtensionMessage)
import ExtensionMessage

port fromGUI : Signal FromGuiMessage

port toGUI : Signal ToGuiMessage
port toGUI = map (ToGuiMessage.encode << .common) outputState

port fromDevice : Signal FromDeviceMessage

port toDevice : Signal ToDeviceMessage
port toDevice = map (\(m,_,_) -> m) output

outputState : Signal BackgroundState
outputState = map (\(_,_,s) -> s) output

output : Signal (ToDeviceMessage, ToExtensionMessage, BackgroundState)
output =
    let go s =
        let (deviceMessage, a1) = DeviceMessage.encode s
            (extMessage, a2)    = ExtensionMessage.encode (update a1 s)
        in (deviceMessage, extMessage, update a2 (update a1 s))
    in map go inputState

port fromExtension : Signal FromExtensionMessage

port toExtension : Signal ToExtensionMessage
port toExtension = map (\(_,m,_) -> m) output

inputState : Signal BackgroundState
inputState =
    foldp update default
        <| mergeMany
            [ map DeviceMessage.decode fromDevice
            , map (CommonAction << FromGuiMessage.decode) fromGUI
            , map ExtensionMessage.decode fromExtension
            ]
