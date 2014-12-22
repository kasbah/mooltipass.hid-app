import Text(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Color
import Signal(..)
import Mouse
import Window
import List

type Screen = Log | Settings | Manage | Developer

type alias State =
    { connected : Bool
    , screen    : Screen
    }

defaultState : State
defaultState =
    { connected = False
    , screen    = Log
    }

main : Signal Element
main = app <~ Window.dimensions ~ foldp (\_ s -> not s) False Mouse.clicks

app : (Int, Int) -> Bool -> Element
app dims connected = let state = {defaultState | connected <- connected}
                     in  scene dims state

scene : (Int, Int) -> State -> Element
scene (w',h') state =
    let (w,h)          = (toFloat w', toFloat h')
        withMargin x   = x - 2*(margin x)
        header'        = header (withMargin w) state.connected
        console'       = console (withMargin w) ((withMargin h) - (64 + 64))
        connectButton' = connectButton (withMargin w) ((withMargin h) - (64 + 64))
    in if not state.connected
       then withMargins w h <| flow down [ header'
                                         , connectButton'
                                         ]
       else withMargins w h <| flow down [ header'
                                         , console'
                                         , clearButton (withMargin w)
                                         ]

header : Float -> Bool -> Element
header w connected =
    let header' = if connected
                  then flow right [ logo w "blue"
                                  , menuButton w
                                  ]
                  else flow right [ logo w "red"
                                  ]
    in container (round w) 64 midLeft header'

console : Float -> Float -> Element
console w h = collage (round w) (round h) [filled grey <| roundedRect w h (max w h/80)]

logo : Float -> String -> Element
logo w color =
    let aspect = 3.394144559879574
        logo'  = image (round (48 * aspect)) 48 ("images/logo-" ++ color ++ ".svg")
    in  container (max (round (48 * aspect)) (round (w * 0.749))) 48 midLeft logo'

menuButton : Float -> Element
menuButton w =
    let aspect = 1.548105123408364
        button = image (round (19 * aspect)) 19 ("images/menu_button.svg")
    in  container (max 48 (round (w * 0.249))) 48 middle button

clearButton : Float -> Element
clearButton w =
    let aspect = 2.96658357613427
        button = image (round (24 * aspect)) 24 ("images/clear_button.svg")
    in  container (round w) 48 midBottom button

connectButton : Float -> Float -> Element
connectButton w h =
    let aspect = 3.8123121543185907
        button = image (round (24 * aspect)) 24 ("images/connect_button.svg")
    in  container (round w) (round h) middle button

blue : Color.Color
blue = Color.rgb 0x0C 0xFE 0xFF

grey : Color.Color
grey = Color.rgb 0x1A 0x1A 0x1A

spacer' : Float -> Element
spacer' x = spacer (round x) (round x)

margin : Float -> Float
margin x = min 30 (x/60)

withMargins : Float -> Float -> Element -> Element
withMargins w h x = flow right [spacer' (margin w), x, spacer' (margin w)]

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


