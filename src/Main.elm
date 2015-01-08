-- Elm standard library
import Graphics.Element (..)
import Signal (..)
import Time (..)
import Window
import Graphics.Input -- needed because of elm bug

-- local source
import State
import Scene
import Communication
import Actions (..)

port toGUI : Signal Communication.Message

port toBackground : Signal Communication.Message
port toBackground = Communication.encode <~ subscribe guiActions

actions : Signal State.Action
actions = mergeMany [ subscribe guiActions
                    , map Communication.decode toGUI
                    ]

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element. -}
main : Signal Element
main = Scene.scene <~ Window.dimensions ~ State.state actions
