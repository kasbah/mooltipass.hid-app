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
port toGUI = map (ToGuiMessage.encode << .common) (dropRepeats state)

port fromDevice : Signal FromDeviceMessage

port toDevice : Signal ToDeviceMessage
port toDevice =
    merge
        (map (\(m,_,_) -> m) output)
        -- this is the keep-alive
        <| map2
            (\_ s -> if | mediaImportActive s && s.deviceConnected
                            -> emptyToDeviceMessage
                        | s.deviceConnected   -> sendCommand OutgoingGetStatus
                        | otherwise           -> emptyToDeviceMessage
            )
            (every second)
            state

port toChrome : Signal ToChromeMessage
port toChrome = map ChromeBgMessage.encode state

port fromChrome : Signal FromChromeMessage

state : Signal BackgroundState
state = map (\(_,_,s) -> s) output

output : Signal (ToDeviceMessage, ToExtensionMessage, BackgroundState)
output =
    let go ias (dm,em,s) =
        let s'                   = apply ias s
            (deviceMessage, a1s) = DeviceMessage.encode s'
            s''                  = apply a1s s'
            (extMessage, a2)     = ExtensionMessage.encode s''
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
