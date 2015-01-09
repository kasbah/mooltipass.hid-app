module Background where

-- Elm standard library
import Signal (..)
import Graphics.Element (..)
import Time (every, second)
import List

-- local source
import Message
import State (..)

type alias MpMessage = {appendToLog : String}

port fromGUI : Signal Message.Message

port fromMP  : Signal MpMessage

mpDecode : MpMessage -> BgState -> BgState
mpDecode msg s =  {s | log <- s.log `List.append` [msg.appendToLog]}

state : Signal BgState
state = foldp mpDecode defaultBgState fromMP

port toGUI : Signal Message.Message
port toGUI = Message.encode <~ merge state (Message.decode <~ fromGUI)

main : Signal Element
main = constant empty
