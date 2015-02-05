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
import ToGuiMessage (..)
import FromGuiMessage
import FromGuiMessage (..)
import Actions (..)
import GuiState (..)
import ChromeMessage (..)
import ChromeMessage

{-| Any state updates from the background are received through this port -}
port fromBackground : Signal ToGuiMessage

{-| Any actions to the common state are first passed to the background. They
    should bubble back up throught the 'port fromBackground'. -}
port toBackground : Signal FromGuiMessage
port toBackground = FromGuiMessage.encode <~ (subscribe commonActions)

port toChrome : Signal ToChromeMessage
port toChrome = map (\(m,_) -> m) output

{-| The complete application state signal to map our main element to. It is
    the gui-state updated by any state updates from the background. -}
state : Signal GuiState
state = map (\(_,s) -> s) output

output : Signal (ToChromeMessage, GuiState)
output =
    let go inputActions (_, s) =
        let s' = apply inputActions s
            (tcm, a) = ChromeMessage.encode s'
        in (tcm, update a s')
    in foldp go
        (emptyToChromeMessage, default)
        inputActions

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element.
-}
main : Signal Element
main = Scene.scene <~ Window.dimensions ~ state

inputActions : Signal (List Action)
inputActions = mergeMany
    [ map ((List.map CommonAction) << ToGuiMessage.decode) fromBackground
    , map (\m -> [m]) (subscribe guiActions)
    ]
