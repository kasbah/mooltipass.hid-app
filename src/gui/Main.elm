module GUI where
-- Elm standard library
import Graphics.Element (..)
import Signal (..)
import Time (..)
import Window
import List

-- local source
import Scene
import ToGuiMessage
import ToGuiMessage (ToGuiMessage)
import FromGuiMessage
import FromGuiMessage (FromGuiMessage)
import Actions (..)
import GuiState (..)
import CommonState as Common

{-| Any state updates from the background are received through this port -}
port fromBackground : Signal ToGuiMessage

{-| Any updates to guiState.common are passed to the background -}
port toBackground : Signal FromGuiMessage
port toBackground = FromGuiMessage.encode <~ (subscribe commonActions)

{-| The complete application state signal to map our main element to. It is the
    gui-state updated by any state updates from the background. -}
state : Signal GuiState
state =
    foldp apply default
        <| merge ((\m -> [m]) <~ (subscribe guiActions))
        <| (List.map CommonAction) <~ (ToGuiMessage.decode <~ fromBackground)

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element. -}
main : Signal Element
main = Scene.scene <~ Window.dimensions ~ state
