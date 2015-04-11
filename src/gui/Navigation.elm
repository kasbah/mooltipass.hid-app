module Navigation where

-- Elm standard library
import Graphics.Element (..)
import Graphics.Input (..)
import Signal (..)
import List

-- local source
import Layout (heights)
import CommonState (..)
import GuiState (..)
import Actions (guiActions)

{-| A tab navigation up top and a Mooltipass status icon on the top right. -}
navigation : (Int, Int) -> GuiState -> Element
navigation (w,h) state =
    flow right
        [ container (round (toFloat w * 0.85)) heights.nav midLeft
            (flow right [navSpacer 38, tabs state])
        , container (round (toFloat w * 0.15)) heights.nav midRight
            (flow left [navSpacer 32, statusIcon state.common.deviceStatus, navSpacer 9000])
        ]

{-| An icon that indictes the connection status and can be clicked 7 times to
    enable developer mode. -}
statusIcon : DeviceStatus -> Element
statusIcon c =
    let aspect          = 1.3285316308250572
        width           = round (toFloat heights.icon * aspect)
        img color       = image width heights.icon
                            ("images/status_icon-" ++ color ++ ".svg")
        clickIcon color = clickable (send guiActions ClickIcon) (img color)
        icon            = case c of
            Unlocked     -> clickIcon "blue"
            ManageMode   -> clickIcon "manage"
            UnknownCard  -> clickIcon "manage"
            NotConnected -> clickIcon "red"
            NoCard       -> clickIcon "orange"
            Locked       -> clickIcon "purple"
    in flow down [icon, spacer 1 heights.iconPadding, navLine width]

{-| A spacer that has a grey line at the bottom -}
navSpacer : Int -> Element
navSpacer w = container w heights.tab bottomLeft (navLine w)

{-| A grey line -}
navLine : Int -> Element
navLine w = tiledImage w 1 "images/tab_spacer_pixel.png"

{-| The tab navigation with an optional developer tab. -}
tabs : GuiState -> Element
tabs state =
    let disabled = disabledTabs state.common.deviceStatus
    in flow right <| [ tab Log      state.activeTab disabled
                     -- disabled for alpha release
                     --, navSpacer 5
                     --, tab Settings state.activeTab disabled
                     , navSpacer 5
                     , tab Manage   state.activeTab disabled
                     ] ++ ( if state.devEnabled
                            then [ navSpacer 5
                                 , tab Developer state.activeTab disabled
                                 ]
                            else [] )
                     ++ [navSpacer 9000]

{-| Tab button which is rendered appropriately depending on if it is active,
    inactive or disabled. -}
tab : Tab -> Tab -> (List Tab) -> Element
tab t active disabled =
    let aspect         = 3.094594610699232
        name = case t of
            Log       -> "log"
            Settings  -> "settings"
            Manage    -> "manage"
            Developer -> "developer"
        img t =
            image (round (toFloat heights.tab * aspect))
                heights.tab
                    ("images/tab_" ++ name ++ "-" ++ t ++ ".svg")
        up             = img "inactive"
        hover          = img "hover"
        down           = img "inactive"
        disabledButton = img "disabled"
        activeButton   = img "active"
        button         = customButton (send guiActions (ChangeTab t)) up hover down
    in  if  | List.member t disabled -> disabledButton
            | t == active            -> activeButton
            | otherwise              -> button
