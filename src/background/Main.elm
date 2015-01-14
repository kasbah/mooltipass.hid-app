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

type alias MpMessage = { appendToLog  : Maybe String
                       , setConnected : Maybe String
                       }

mpDecode : MpMessage -> CommonAction
mpDecode message =
    let decode {appendToLog, setConnected} =
        Maybe.oneOf [ Maybe.map AppendToLog appendToLog
                    , Maybe.map connectedFromInt setConnected
                    ]
        connectedFromInt s =
            case s of
                "NotConnected" -> SetConnected NotConnected
                "Connected"    -> SetConnected Connected
                "NoCard"       -> SetConnected NoCard
                "NoPin"        -> SetConnected NoPin
                _              -> CommonNoOp
    in Maybe.withDefault CommonNoOp (decode message)

port fromGUI : Signal FromGuiMessage

port fromMP  : Signal MpMessage

state : Signal CommonState
state =
    foldp update default
        <| merge (mpDecode <~ fromMP)
        <| FromGuiMessage.decode <~ fromGUI

port toGUI : Signal ToGuiMessage
port toGUI = ToGuiMessage.encode <~ (dropRepeats state)

--deviceActions : Channel (List Int)
--deviceActions = channel []
--
--port toDevice : Signal (List Int)
--port toDevice = subscribe deviceActions

main : Signal Element
main = constant empty
