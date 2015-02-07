module Background where

-- Elm standard library
import Signal (..)
import Time (..)
import List ((::))

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

tick = merge (map (\_ -> ()) (every (100*millisecond))) (map (\_ -> ()) fromDevice)

port toDevice : Signal ToDeviceMessage
port toDevice = map (\(m,_,_) -> m) (sampleOn tick output)
        -- <| map2
        --     (\_ s -> if s.deviceConnected
        --              then sendCommand AppGetStatus
        --              else {emptyToDeviceMessage | connect <- Just ()})
        --     (every second)
        --     state

port toChrome : Signal ToChromeMessage
port toChrome = map ChromeBgMessage.encode state

port fromChrome : Signal FromChromeMessage

state : Signal BackgroundState
state = map (\(_,_,s) -> s) output

output : Signal (ToDeviceMessage, ToExtensionMessage, BackgroundState)
output =
    let go inputActions (dm,em,s) =
        let s'                  = apply inputActions s
            (deviceMessage, a1s) = DeviceMessage.encode s'
            s''                 = apply a1s s'
            (extMessage, a2)    = ExtensionMessage.encode s''
        in (deviceMessage, extMessage, update a2 s'')
    in foldp go (emptyToDeviceMessage, emptyToExtensionMessage, default) inputActions

port fromExtension : Signal FromExtensionMessage

port toExtension : Signal ToExtensionMessage
port toExtension = map (\(_,m,_) -> m) output

inputActions : Signal (List BackgroundAction)
inputActions = mergeMany
    [ map (\m -> [CommonAction (FromGuiMessage.decode m)]) fromGUI
    , map (\m -> [ExtensionMessage.decode m]) fromExtension
    , map (\m -> [ChromeBgMessage.decode m]) fromChrome
    , map DeviceMessage.decode fromDevice
    ]
