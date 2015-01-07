-- Elm standard library
import Graphics.Element (..)
import Signal (..)
import Time (..)
import Window

-- local source
import State
import Scene

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element. -}
main : Signal Element
main = Scene.scene <~ Window.dimensions ~ State.state
