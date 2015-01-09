module GUI where
-- Elm standard library
import Graphics.Element (..)
import Signal (..)
import Time (..)
import Window

-- local source
import Scene
import Message
import Actions (..)
import State (..)

port fromBackground : Signal Message.Message

port toBackground : Signal Message.Message
port toBackground =
    (Message.encode << toBgState) <~ state

{-| The state signal to map our main element to -}
state : Signal GuiState
state = foldp update default (subscribe guiActions)

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element. -}
main : Signal Element
main = Scene.scene <~ Window.dimensions
                   ~ (fromBgState <~ (Message.decode <~ fromBackground) ~ state)
