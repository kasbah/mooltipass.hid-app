module GUI where
-- Elm standard library
import Graphics.Element (..)
import Signal (..)
import Time (..)
import Window
import List

-- local source
import Scene
import Message
import Actions (..)
import GuiState (..)

{-| Any state updates from the background are received through this port -}
port fromBackground : Signal Message.Message

{-| Any updates to guiState.common are passed to the background -}
port toBackground : Signal Message.Message
port toBackground = (Message.encode << .common) <~ state

{-| The state signal of the GUI. This is only dependant on user interaction. -}
guiState : Signal GuiState
guiState = foldp update default (subscribe guiActions)

{-| The complete application state signal to map our main element to. It is the
    gui-state updated by any state updates from the background. -}
state : Signal GuiState
state = dropRepeats <|
    (\comActions gui -> apply (List.map CommonAction comActions) gui)
        <~ (Message.decode <~ fromBackground) ~ guiState

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element. -}
main : Signal Element
main = Scene.scene <~ Window.dimensions ~ state
