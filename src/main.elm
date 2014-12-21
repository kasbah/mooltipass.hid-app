import Text(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Color
import Signal(..)
import Mouse
import Window

main = scene <~ Window.dimensions ~ foldp (\_ s -> not s) False Mouse.clicks

scene (w',h') connected =
    let (w,h) = (toFloat w', toFloat h')
    in withMargins w h <| header (w - (2*(margin w))) (h - (2*(margin h))) connected

header w h connected =
    if connected then
        flow right [ logo w "blue"
                   , menuButton w
                   ]
                 else
        logo w "red"

logo w color =
    let aspect = 3.394144559879574
        logo'  = image (round (85 * aspect)) 85 ("images/logo-" ++ color ++ ".svg")
    in  container (round (w/1.5)) 85 midLeft logo'

menuButton w =
    let aspect = 1.548105123408364
        button = image (round (32 * aspect)) 32 ("images/menu_button.svg")
    in  container (round (w/2.5)) 85 middle button

blue = Color.rgb 0x0C 0xFE 0xFF
spacer' x = spacer (round x) (round x)
margin x = x/20

withMargins w h x = flow down [spacer' (margin h), flow right [spacer' (margin w), x, spacer' (margin w)], spacer' (margin h)]




