module Content where

-- Elm standard library
import Graphics.Element (..)
import Graphics.Collage (..)
import Graphics.Input (..)
import Text (..)
import Signal (..)

-- local source
import Layout (..)
import State (..)
import CustomGraphics (..)

{-| Renders the window the window dimensions and application state to the
    element that is below the tab navigation. -}
content : (Int, Int) -> State -> Element
content (w,h) state =
    let h' = h - heights.marginTop - heights.nav - heights.marginBottom
        w' = w - (32 * 2)
        background = collage w h [filled darkGrey (rect (toFloat w) (toFloat h))]
    in layers [background, console (w, h') state.log]

{-| Displays the log in a console screen with a clear button at the bottom in a
    'toolbar' -}
console : (Int, Int) -> String -> Element
console (w,h) log =
    let (w',h')          = (toFloat w, toFloat h)
        screenH          = h - heights.consoleToolbar - 32
        screenH'         = toFloat screenH
        screenW          = w - 64
        screenW'         = toFloat screenW
        screenBackground = collage w screenH
                            [filled grey
                                <| roundedRect screenW' screenH'
                                <| (max screenW' screenH') / 80
                            ]
        screenText       = leftAligned <| fromString log
        screen           = layers [screenBackground, screenText]
        toolbar          = container w heights.consoleToolbar middle clearButton
    in container w h middle <| flow down [screen, toolbar]

{-| A button that says 'clear' and posts a 'State.ClearLog' action -}
clearButton : Element
clearButton =
    let aspect = 2.96658357613427
        img t =
            image (round (toFloat heights.consoleButton * aspect))
                heights.consoleButton
                    ("images/button_clear" ++ "-" ++ t ++ ".svg")
        up     = img "up"
        hover  = img "hover"
        down   = img "down"
    in  customButton (send actions ClearLog) up hover down
