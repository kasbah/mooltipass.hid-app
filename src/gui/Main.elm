module GUI where
-- Elm standard library
import Graphics.Element (..)
import Signal (..)
import Time (..)
import Window

-- local source
import State
import Scene
import Message
import Actions (..)

port fromBackground : Signal Message.Message

port toBackground : Signal Message.Message
port toBackground =
    Message.encode <~ (State.toBgState <~ State.state (subscribe guiActions))

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element. -}
main : Signal Element
main = Scene.scene <~ Window.dimensions
                   ~ (State.fromBgState <~ (Message.decode <~ fromBackground) ~ State.state (subscribe guiActions))
