import Color
import Graphics.Collage(..)
import Graphics.Element(..)
import Graphics.Input(..)
import List
import Mouse
import Signal(..)
import Text
import Window
import CustomGraphics (roundedRect)

main : Signal Element
main = scene <~ Window.dimensions ~ (foldp update defaultState (subscribe actions))

-- State
type Tab = Log | Settings | Manage | Developer
type ConnectState = NotConnected | Connected | NoCard | NoPin

{-| The entire application state -}
type alias State =
    { connected : ConnectState
    , activeTab    : Tab
    }

defaultState : State
defaultState =
    { connected = Connected
    , activeTab = Log
    }

{-| All actions that can be performed to change state -}
type Action = ChangeTab Tab | SetConnected ConnectState | NoOp

{-| Transform the state to a new state according to an action -}
update : Action -> State -> State
update action state =
    case action of
        (ChangeTab t)    -> {state | activeTab <- t}
        (SetConnected c) -> {state | connected <- c}
        NoOp             -> state

actions : Channel Action
actions = channel NoOp

-- Scene
scene : (Int,Int) -> State -> Element
scene dims state = layers [layer1 dims state]

layer1 : (Int, Int) -> State -> Element
layer1 dims state =
    flow down [ title dims state
              , spacer 16 16
              , navigation dims state
              --, content dims state
              ]

title : (Int, Int) -> State -> Element
title (w,h) state =
    let logo' = case state.connected of
        Connected    -> logo "blue"
        NotConnected -> logo "red"
        NoCard       -> logo "orange"
        NoPin        -> logo "orange"
    in container w 64 middle logo'

logo : String -> Element
logo color =
    let aspect = 3.394144559879574
    in  image (round (38 * aspect)) 38 ("images/logo-" ++ color ++ ".svg")

navigation : (Int, Int) -> State -> Element
navigation (w,h) state =
    container w 38 midLeft (flow right [spacer 32 38, tabs state])

tabs : State -> Element
tabs state =
    let disabled = case state.connected of
        Connected    -> []
        NotConnected -> [Settings, Manage]
        NoCard       -> [Settings, Manage]
        NoPin        -> [Manage]
    in flow right [ tab Log      state.activeTab disabled
                  , spacer 5 5
                  , tab Settings state.activeTab disabled
                  , spacer 5 5
                  , tab Manage   state.activeTab disabled
                  ]

tab : Tab -> Tab -> (List Tab) -> Element
tab t active disabled =
    let aspect         = 3.094594610699232
        name = case t of
            Log      -> "log"
            Settings -> "settings"
            Manage   -> "manage"
        up             = image (round (32 * aspect)) 32 ("images/tab_" ++ name ++ "-inactive.svg")
        hover          = image (round (32 * aspect)) 32 ("images/tab_" ++ name ++ "-active.svg")
        down           = image (round (32 * aspect)) 32 ("images/tab_" ++ name ++ "-active.svg")
        disabledButton = image (round (32 * aspect)) 32 ("images/tab_" ++ name ++ "-disabled.svg")
        activeButton   = image (round (32 * aspect)) 32 ("images/tab_" ++ name ++ "-active.svg")
        button         = customButton (send actions (ChangeTab t)) up hover down
    in  if List.member t disabled
        then disabledButton
        else if t == active
             then activeButton
             else button


--connectButton : Float -> Float -> Element
--connectButton w h =
--    let aspect = 3.3747858968383753
--        up     = image (round (24 * aspect)) 24 ("images/connect_button.svg")
--        hover  = image (round (24 * aspect)) 24 ("images/connect_button_hover.svg")
--        down   = image (round (24 * aspect)) 24 ("images/connect_button_down.svg")
--        button = customButton (send actions (SetConnected True)) up hover down
--    in  container (round w) (round h) middle button
--
--popup : (Int, Int) -> Element -> State -> Element
--popup (w',h') hdr state =
--    let aspect = 0.7009962578555462
--    in  (image 200 (round (200 / aspect)) "images/popup.svg") `below` hdr
--
--header : Float -> Bool -> Element
--header w connected =
--    let header' = if connected
--                  then flow right [ logo w "blue"
--                                  , menuButton w
--                                  ]
--                  else flow right [ logo w "red"
--                                  ]
--    in container (round w) 64 midLeft header'
--
--console : Float -> Float -> Element
--console w h = collage (round w) (round h) [filled grey <| roundedRect w h (max w h/80)]
--
--
--menuChannel : Channel ()
--menuChannel = channel ()
--
--menuButton : Float -> Element
--menuButton w =
--    let aspect = 1.4404321532671787
--        up     = image (round (24 * aspect)) 24 ("images/menu_button.svg")
--        hover  = image (round (24 * aspect)) 24 ("images/menu_button_hover.svg")
--        down   = image (round (24 * aspect)) 24 ("images/menu_button_down.svg")
--        button = customButton (send clearChannel ()) up hover down
--    in  container (max 48 (round (w * 0.249))) 48 middle button
--
--clearChannel : Channel ()
--clearChannel = channel ()
--
--clearButton : Float -> Element
--clearButton w =
--    let aspect = 2.96658357613427
--        up     = image (round (24 * aspect)) 24 ("images/clear_button.svg")
--        hover  = image (round (24 * aspect)) 24 ("images/clear_button_hover.svg")
--        down   = image (round (24 * aspect)) 24 ("images/clear_button_down.svg")
--        button = customButton (send clearChannel ()) up hover down
--    in  container (round w) 48 midBottom button
--
--connectChannel : Channel ()
--connectChannel = channel ()
--
--
--blue : Color.Color
--blue = Color.rgb 0x0C 0xFE 0xFF
--
--grey : Color.Color
--grey = Color.rgb 0x1A 0x1A 0x1A
--
--spacer' : Float -> Element
--spacer' x = spacer (round x) (round x)
--
--margin : Float -> Float
--margin x = min 30 (x/60)
--
--withMargins : Float -> Float -> Element -> Element
--withMargins w h x = flow right [spacer' (margin w), x, spacer' (margin w)]
