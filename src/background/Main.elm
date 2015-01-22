module Background where

-- Elm standard library
import Signal (..)
import Time (..)

-- local source
import ToGuiMessage
import ToGuiMessage (ToGuiMessage)
import FromGuiMessage
import FromGuiMessage (FromGuiMessage)
import CommonState (CommonAction)
import DeviceMessage (..)
import DeviceMessage
import BackgroundState (..)
import ExtensionMessage (..)
import ExtensionMessage
import DevicePacket (..)

port fromGUI : Signal FromGuiMessage

port toGUI : Signal ToGuiMessage
port toGUI = map (ToGuiMessage.encode << .common) outputState

port fromDevice : Signal FromDeviceMessage

port toDevice : Signal ToDeviceMessage
port toDevice =
    merge
        (map (\(m,_,_) -> m) output)
        <| map2
            (\_ s -> if s.hidConnected
                     then sendCommand AppGetStatus
                     else emptyToDeviceMessage)
            (every second)
            outputState

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
