module Content where

-- Elm standard library
import Color
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Input (..)
import Signal (..)
import Html
import Html.Attributes
import Text -- needed even when unused because of Elm bug #864
import List

-- local source
import Layout (..)
import State (..)
import CustomGraphics (..)
import Actions (..)

{-| Renders the window the window dimensions and application state to the
    element that is below the tab navigation. -}
content : (Int, Int) -> State -> Element
content (w,h) state =
    let h' = h - heights.marginTop - heights.nav - heights.marginBottom
        background =
            collage w h' [filled darkGrey <| rect (toFloat w) (toFloat h)]
    in layers [background, console (w, h') state.log]

{-| Displays the log in a console screen with a clear button at the bottom in a
    'toolbar'. -}
console : (Int, Int) -> List String -> Element
console (w,h) log =
    let toolbar = container w heights.consoleToolbar middle clearButton
        screenH = h - heights.consoleToolbar - 32
        screenW = w - 64
        screen' =
            container w screenH middle <| screen (screenW, screenH) log
    in container w h middle <| flow down [screen', toolbar]

{-| The console screen that displays the log string. -}
screen : (Int, Int) -> List String -> Element
screen (w,h) log =
    let (w',h')    = (toFloat w, toFloat h)
        background = collage w h
                            [filled grey
                                <| roundedRect w' h'
                                <| (max w' h') / 80
                            ]
        style =
            Html.Attributes.style
                [ ("color", "white")
                , ("font-family", "DejaVu Sans Mono")
                , ("-webkit-user-select", "text")
                , ("font-size", "13px")
                , ("overflow-y", "auto")
                , ("width", toString (w - 32) ++ "px")
                , ("height", toString (h - 32) ++ "px")
                ]
        txt  = Html.div [style] (List.intersperse (Html.br [] []) (List.map Html.text log))
                |> Html.toElement (w - 32) (h - 32)
        txt' = container w h middle txt
    in layers [background, txt']

{-| A button that says 'clear' and posts a 'State.ClearLog' action -}
clearButton : Element
clearButton =
    let aspect = 2.96658357613427
        img t =
            image (round (toFloat heights.consoleButton * aspect))
                heights.consoleButton
                    ("images/button_clear-" ++ t ++ ".svg")
        up     = img "up"
        hover  = img "hover"
        down   = img "down"
    in  customButton (send guiActions (SetLog [])) up hover down
