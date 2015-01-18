module Background where

-- Elm standard library
import Signal (..)
import Graphics.Element (..)
import Time (every, second)
import List
import List ((::))
import Maybe

-- local source
import ToGuiMessage
import ToGuiMessage (ToGuiMessage)
import FromGuiMessage
import FromGuiMessage (FromGuiMessage)
import CommonState (..)

type alias DeviceMessage = { setConnected : Maybe String
                           }

mpDecode : DeviceMessage -> CommonAction
mpDecode message =
    let decode {setConnected} =
        Maybe.oneOf [ Maybe.map connectedFromString setConnected
                    ]
        connectedFromString s =
            case s of
                "NotConnected" -> SetConnected NotConnected
                "Connected"    -> SetConnected Connected
                "NoCard"       -> SetConnected NoCard
                "NoPin"        -> SetConnected NoPin
                _              -> CommonNoOp
    in Maybe.withDefault CommonNoOp (decode message)

port fromGUI : Signal FromGuiMessage

port fromDevice : Signal DeviceMessage

port toDevice : Signal Bool
port toDevice =
    let toDevice' s = case s.connected of
        NotConnected -> False
        _            -> True
    in toDevice' <~ state

state : Signal CommonState
state =
    foldp update default
        <| merge (mpDecode <~ fromDevice)
        <| FromGuiMessage.decode <~ fromGUI

port toGUI : Signal ToGuiMessage
port toGUI = ToGuiMessage.encode <~ state

--deviceActions : Channel (List Int)
--deviceActions = channel []
--
--port toDevice : Signal (List Int)
--port toDevice = subscribe deviceActions

main : Signal Element
main = constant empty
