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
import CommonState as Common

{-| Any state updates from the background are received through this port -}
port fromBackground : Signal ToGuiMessage

{-| Any actions to the common state are first passed to the background. They
    should bubble back up throught the 'port fromBackground'. -}
port toBackground : Signal FromGuiMessage
port toBackground =
        (map FromGuiMessage.encode (subscribe commonActions))

port toChrome : Signal ToChromeMessage
port toChrome = map (\(m,_,_) -> m) output

port fromChrome : Signal FromChromeMessage

{-| The complete application state signal to map our main element to. It is
    the gui-state updated by any state updates from the background. -}
state : Signal GuiState
state = map (\(_,_,s) -> s) output

forbg : GuiState -> (FromGuiMessage, Action)
forbg s =
    let e = emptyFromGuiMessage
    in case s.importMedia of
        RequestFile p ->
            ( FromGuiMessage.encode (Common.StartImportMedia p)
            , SetImportMedia NotRequested)
        _             -> (e, NoOp)

output : Signal (ToChromeMessage, FromGuiMessage, GuiState)
output =
    let go inputActions (_,_,s) =
        let s'        = apply inputActions s
            (tcm, a1) = ChromeMessage.encode s'
            s''       = update a1 s'
            (fgm, a2) = forbg s''
        in (tcm, fgm, update a2 s'')
    in foldp go
        (emptyToChromeMessage, emptyFromGuiMessage, default)
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
    , map ((\m -> [m]) << ChromeMessage.decode) fromChrome
    ]
