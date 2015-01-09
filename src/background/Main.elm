module Background where

-- Elm standard library
import Signal (..)
import Graphics.Element (..)
import Time (every, second)

-- local source
import Message
import State (..)

port fromGUI : Signal Message.Message

state : Signal BgState
state = Message.decode <~ fromGUI

port toGUI : Signal Message.Message
port toGUI = Message.encode <~ state

main : Signal Element
main = constant empty
