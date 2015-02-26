module CustomGraphics where
-- Elm standard library
import List
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Element as Element
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

{-| A rounded rectangle element with a given width, height and a 5 degree
    corner radius -}
roundedRect : Int -> Int -> Color.Color -> Element
roundedRect w h c =
    collage w h [filled c <| roundedRectShape All (toFloat w) (toFloat h) 5]

type RoundedSide = Top | Left | Right | Bottom | All

roundedRectShape : RoundedSide -> Float -> Float -> Float -> Shape
roundedRectShape side  w h r =
  let hw = w/2
      hh = h/2
      (a,b,c,d) = case side of
        Top    -> (False,True ,True ,False)
        Bottom -> (True ,False,False,True)
        Left   -> (True ,True ,False,False)
        Right  -> (False,False,True ,True)
        _      -> (True ,True ,True ,True)
      aa = if a then (arc (0-hw+r, 0-hh+r) (r, r) (270 |> degrees, 180 |> degrees))
           else [(0,-hh),(-hw,-hh),(-hw,0)]
      bb = if b then (arc (0-hw+r, hh-r) (r, r) (180 |> degrees, 90 |> degrees))
           else [(-hw,0),(-hw,hh),(0,hh)]
      cc = if c then (arc (hw-r, hh-r) (r, r) (90 |> degrees, 0 |> degrees))
           else [(0,hh),(hw,hh),(hw,0)]
      dd = if d then (arc (hw-r, 0-hh+r) (r, r) (0 |> degrees, -90 |> degrees))
           else [(hw,0),(hw,-hh),(0,-hh)]
  in aa ++ bb ++ cc ++ dd

-- Basic colourscheme
grey : Color.Color
grey = Color.rgb 0x1A 0x1A 0x1A

darkGrey : Color.Color
darkGrey = Color.rgb 0x10 0x10 0x10

darkGrey' : Color.Color
darkGrey' = Color.rgb 0x20 0x20 0x20

lightGrey : Color.Color
lightGrey = Color.rgb 0x30 0x30 0x30

lightGrey' : Color.Color
lightGrey' = Color.rgb 0x40 0x40 0x40

lightGrey'' : Color.Color
lightGrey'' = Color.rgb 0x50 0x50 0x50

cyan : Color.Color
cyan = Color.rgb 0x00 0xff 0xd8

blue : Color.Color
blue = Color.rgb 0x00 0x84 0xff

whiteText : String -> Text
whiteText str =
    Text.style {defaultStyle' | color <- Color.white} (fromString str)

text : String -> Text
text str = Text.style defaultStyle' (fromString str)

defaultStyle' = {defaultStyle | typeface <- ["DejaVu Sans Mono"]}

button : Signal.Message -> String -> Element
button msg str = button' 2.96658357613427 "button" msg str

bigButton : Signal.Message -> String -> Element
bigButton msg str = button' 6.018072289156627 "bigButton" msg str

disabledButton str =
    let h      = heights.button
        w      = round (toFloat h * aspect)
        aspect = 2.96658357613427
        th = 11
        txt = Element.width w
            <| centered <| Text.height th <| whiteText str
        centeredText = container (w - 2) h middle txt
    in layers [image w h ("images/button-disabled.svg"), centeredText]

button' aspect src msg str =
    let h = heights.button
        w = round (toFloat h * aspect)
        hDown = heights.button - 2
        wDown = round (toFloat hDown * aspect)
        img w' h' t = image w' h' ("images/" ++ src ++ "-" ++ t ++ ".svg")
        txt w' th = Element.width w'
            <| centered <| Text.height th <| whiteText str
        centeredText w' h' th = container (w' - 2) h' middle (txt w' th)
        up     = layers [img w h "up"   , centeredText w h 11]
        hover  = layers [img w h "hover", centeredText w h 11]
        down   = container w h middle
            <| layers
                [img wDown hDown "hover", centeredText wDown hDown 10]
    in  customButton msg up hover down
