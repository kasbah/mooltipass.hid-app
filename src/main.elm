import Text(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Color
import Signal(..)
import Mouse
import Window

blue = Color.rgb 0x0C 0xFE 0xFF

leftMargin = spacer 20 20
topMargin  = spacer 20 20

logo w h color =
    let aspect = 3.394144559879574
    in  fittedImage (floor (w / 2)) (floor (w / 2 / aspect))
       ("images/logo-" ++ color ++ ".svg")

scene (w',h') = let (w,h) = (toFloat w', toFloat h')
                in flow down [topMargin, flow right [leftMargin, logo w h "red"]]

main = scene <~ Window.dimensions
