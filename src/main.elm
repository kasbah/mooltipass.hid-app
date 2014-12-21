import Text(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Color
import Signal(..)
import Mouse
import Window

blue = Color.rgb 0x0C 0xFE 0xFF
spacer' x = spacer (round x) (round x)
margin x = x/30

logo w h color =
    let aspect = 3.394144559879574
    in scaledImage w h 2 aspect 100 350 ("images/logo-" ++ color ++ ".svg")

withMargins w h x = flow down [spacer' (margin w), flow right [spacer' (margin h), x]]

scaledImage w h scale aspect min max path =
    fittedImage (floor (clamp min max (w / scale))) (floor (clamp (min / aspect) (max / aspect) (w / scale / aspect))) path

menuButton w h =
    let aspect = 1.548105123408364
    in scaledImage w h 10 aspect 25 50 "images/menu_button.svg"

scene (w',h') connected =
    let (w,h) = (toFloat w', toFloat h')
    in  case connected of
        False -> withMargins w h <| logo w h "red"
        True  -> withMargins w h <| flow right [logo w h "blue", spacer' (w/4), menuButton w h]

main = scene <~ Window.dimensions ~ constant True
