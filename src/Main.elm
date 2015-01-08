-- Elm standard library
import Graphics.Element (..)
import Signal (..)
import Time (..)
import Window

-- local source
import State
import Scene
import Communication
import UserActions (userActions)

port toGUI : Signal Communication.Message

actions : Signal State.Action
actions = mergeMany [ subscribe userActions
                    , map Communication.decode toGUI
                    ]

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element. -}
main : Signal Element
main = Scene.scene <~ Window.dimensions ~ State.state actions
