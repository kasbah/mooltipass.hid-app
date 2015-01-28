module CustomGraphics where
-- Elm standard library
import List
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Input (..)
import Text (..)
import Text
import Color
import Signal

import Layout (..)

{-| An elliptical arc with the given center, radii and angle interval. -}
arc : (Float, Float) -> (Float, Float) -> (Float, Float) -> Shape
arc (cx, cy) (a, b) (startAngle, endAngle) =
  let n = 50
      t = (endAngle - startAngle) / n
      f i = (cx + a * cos (t*i + startAngle), cy + b * sin (t*i + startAngle))
  in List.map f [0..n-1]

{-| A rounded rec tangle with a given width, height and corner radius. -}
roundedRect : Float -> Float -> Float -> Shape
roundedRect w h r =
  let hw = w/2
      hh = h/2
  in (arc (0-hw+r, 0-hh+r) (r, r) (270 |> degrees, 180 |> degrees)) ++
     (arc (0-hw+r, hh-r) (r, r) (180 |> degrees, 90 |> degrees)) ++
     (arc (hw-r, hh-r) (r, r) (90 |> degrees, 0 |> degrees)) ++
     (arc (hw-r, 0-hh+r) (r, r) (0 |> degrees, -90 |> degrees)) ++
     [(0-hw+r, 0-hh)]

-- Basic colourscheme
grey : Color.Color
grey = Color.rgb 0x1A 0x1A 0x1A

darkGrey : Color.Color
darkGrey = Color.rgb 0x10 0x10 0x10

blue : Color.Color
blue = Color.rgb 0x0C 0xFE 0xFF

text : String -> Text
text str = Text.style {defaultStyle | typeface <- ["DejaVu Sans Mono"]
                                , color <- Color.white
                      } (fromString str)

button : Signal.Message -> String -> Element
button msg str =
    let aspect = 2.96658357613427
        h = heights.logTabButton
        w = round (toFloat h * aspect)
        hDown = heights.logTabButton - 2
        wDown = round (toFloat hDown * aspect)
        img w' h' t = image w' h' ("images/button-" ++ t ++ ".svg")
        txt th = centered <| Text.height th <| text str
        centeredText w' h' th = container (w' - 4) h' middle (txt th)
        up     = layers [img w h "up"   , centeredText w h 11]
        hover  = layers [img w h "hover", centeredText w h 11]
        down   = container w h middle <| layers [img wDown hDown "hover", centeredText wDown hDown 10]
    in  customButton msg up hover down
