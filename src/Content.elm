module Content where

-- Elm standard library
import Color
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Input (..)
import Signal (..)
import Text
import Text (..)

-- local source
import Layout (..)
import State (..)
import CustomGraphics (..)

{-| Renders the window the window dimensions and application state to the
    element that is below the tab navigation. -}
content : (Int, Int) -> State -> Element
content (w,h) state =
    let h' = h - heights.marginTop - heights.nav - heights.marginBottom
        background =
            collage w h [filled darkGrey <| rect (toFloat w) (toFloat h)]
    in layers [background, console (w, h') state.log]

{-| Displays the log in a console screen with a clear button at the bottom in a
    'toolbar'. -}
console : (Int, Int) -> String -> Element
console (w,h) log =
    let toolbar = container w heights.consoleToolbar middle clearButton
        screenH = h - heights.consoleToolbar - 32
        screenW = w - 64
        screen' =
            container w screenH middle <| screen (screenW, screenH) log
    in container w h middle <| flow down [screen', toolbar]

{-| The console screen that displays the log string. -}
screen : (Int, Int) -> String -> Element
screen (w,h) log =
    let (w',h')    = (toFloat w, toFloat h)
        background = collage w h
                            [filled grey
                                <| roundedRect w' h'
                                <| (max w' h') / 80
                            ]
        txt        = leftAligned <| Text.color Color.white <| fromString log
    in layers [background, txt]

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
    in  customButton (send actions ClearLog) up hover down
