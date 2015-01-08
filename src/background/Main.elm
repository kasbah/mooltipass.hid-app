module Background where
-- Elm standard library
import Signal (..)
import Graphics.Element (..)

-- local source
import Message

port toBackground : Signal Message.Message

port toGUI : Signal Message.Message
port toGUI = constant Message.emptyMessage

main : Signal Element
main = constant empty
