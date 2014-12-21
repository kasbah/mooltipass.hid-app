import Text(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Color
import Signal(..)
import Mouse
import Window

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
    let (w,h) = (toFloat w', toFloat h')
    in withMargins w h <| header (w - (2*(margin w))) state.connected

header : Float -> Bool -> Element
header w connected =
    if connected
    then
        flow right [ logo w "blue"
                   , menuButton w
                   ]
    else
        logo w "red"

logo : Float -> String -> Element
logo w color =
    let aspect = 3.394144559879574
        logo'  = image (round (85 * aspect)) 85 ("images/logo-" ++ color ++ ".svg")
    in  container (max 300 (round (w * 0.749))) 85 midLeft logo'

menuButton : Float -> Element
menuButton w =
    let aspect = 1.548105123408364
        button = image (round (32 * aspect)) 32 ("images/menu_button.svg")
    in  container (max 85 (round (w * 0.249))) 85 middle button

blue : Color.Color
blue = Color.rgb 0x0C 0xFE 0xFF

spacer' : Float -> Element
spacer' x = spacer (round x) (round x)

margin : Float -> Float
margin x = x/20

withMargins : Float -> Float -> Element -> Element
withMargins w h x = flow down [spacer' (margin h), flow right [spacer' (margin w), x, spacer' (margin w)], spacer' (margin h)]




