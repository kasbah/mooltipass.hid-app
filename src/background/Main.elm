module Background where

-- Elm standard library
import Signal (..)
import Graphics.Element (..)
import Time (every, second)
import List
import List ((::))
import Maybe

-- local source
import Message
import CommonState (..)

type alias MpMessage = { appendToLog  : Maybe String
                       , setConnected : Maybe Int
                       }

mpDecode : MpMessage -> CommonAction
mpDecode message =
    let decode' {appendToLog, setConnected} =
        Maybe.oneOf [ Maybe.map AppendToLog appendToLog
                    , Maybe.map connectedFromInt setConnected
                    ]
        connectedFromInt s =
            case s of
                0 -> SetConnected NotConnected
                1 -> SetConnected Connected
                2 -> SetConnected NoCard
                3 -> SetConnected NoPin
                _ -> CommonNoOp
    in Maybe.withDefault CommonNoOp (decode' message)

port fromGUI : Signal Message.Message

port fromMP  : Signal MpMessage

state : Signal CommonState
state =
    foldp apply default
        <| merge ((\m -> [m]) <~ (mpDecode <~ fromMP))
        <| Message.decode' <~ fromGUI

port toGUI : Signal Message.Message
port toGUI = Message.encode <~ state

main : Signal Element
main = constant empty
