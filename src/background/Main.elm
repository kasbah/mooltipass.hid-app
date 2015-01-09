module Background where

-- Elm standard library
import Signal (..)
import Graphics.Element (..)
import Time (every, second)
import List
import List ((::))

-- local source
import Message
import CommonState (..)

type alias MpMessage = {appendToLog : String}

port fromGUI : Signal Message.Message

port fromMP  : Signal MpMessage

mpDecode : MpMessage -> List CommonAction
mpDecode {appendToLog} = [AppendToLog appendToLog]

state : Signal CommonState
state = foldp apply default (merge (mpDecode <~ fromMP) (Message.decode' <~ fromGUI))

port toGUI : Signal Message.Message
port toGUI = Message.encode <~ state

main : Signal Element
main = constant empty
