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
import ChromeBgMessage
import ChromeBgMessage (..)

port fromGUI : Signal FromGuiMessage

port toGUI : Signal ToGuiMessage
port toGUI = map (ToGuiMessage.encode << .common) state

port fromDevice : Signal FromDeviceMessage

port toDevice : Signal ToDeviceMessage
port toDevice =
    merge
        (map (\(m,_,_) -> m) output)
        <| map2
            (\_ s -> if s.deviceConnected
                     then sendCommand AppGetStatus
                     else {emptyToDeviceMessage | connect <- Just ()})
            (every second)
            state

port toChrome : Signal ToChromeMessage
port toChrome = map ChromeBgMessage.encode state

port fromChrome : Signal FromChromeMessage

state : Signal BackgroundState
state = map (\(_,_,s) -> s) output

output : Signal (ToDeviceMessage, ToExtensionMessage, BackgroundState)
output =
    let go inputActions (dm,em,s) =
        let s'                  = update inputActions s
            (deviceMessage, a1) = DeviceMessage.encode s'
            s''                 = update a1 s'
            (extMessage, a2)    = ExtensionMessage.encode s''
        in (deviceMessage, extMessage, update a2 s'')
    in foldp go (emptyToDeviceMessage, emptyToExtensionMessage, default) inputActions

port fromExtension : Signal FromExtensionMessage

port toExtension : Signal ToExtensionMessage
port toExtension = map (\(_,m,_) -> m) output

inputActions : Signal BackgroundAction
inputActions = mergeMany
    [ map DeviceMessage.decode fromDevice
    , map (CommonAction << FromGuiMessage.decode) fromGUI
    , map ExtensionMessage.decode fromExtension
    , map ChromeBgMessage.decode fromChrome
    ]
