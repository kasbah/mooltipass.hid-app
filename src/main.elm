import Text(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Color
import Signal(..)
import Mouse
import Window

main = scene <~ Window.dimensions ~ constant True

scene (w',h') connected =
    let (w,h) = (toFloat w', toFloat h')
    in  case connected of
        True  -> withMargins w h <| header (w - (2*(margin w))) (h - (2*(margin h)))

header w h = flow right [ container (round (w/1.5)) 85 midLeft (logo "blue")
                        , container (round (w/2.5)) 85 middle menuButton
                        ]

logo color =
    let aspect = 3.394144559879574
    in image (round (85 * aspect)) 85 ("images/logo-" ++ color ++ ".svg")

menuButton =
    let aspect = 1.548105123408364
    in image (round (32 * aspect)) 32 ("images/menu_button.svg")

blue = Color.rgb 0x0C 0xFE 0xFF
spacer' x = spacer (round x) (round x)
margin x = x/20

withMargins w h x = flow down [spacer' (margin h), flow right [spacer' (margin w), x, spacer' (margin w)], spacer' (margin h)]




