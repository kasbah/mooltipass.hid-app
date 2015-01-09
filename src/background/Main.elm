module Background where
-- Elm standard library
import Signal (..)
import Graphics.Element (..)
import Time (every, second)

-- local source

import Message

port fromGUI : Signal Message.Message

port toGUI : Signal Message.Message
port toGUI = (\_ -> {setLog = ["yoyoyoyo"], setConnected = 0}) <~ every second

main : Signal Element
main = constant empty
