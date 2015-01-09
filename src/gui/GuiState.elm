module GuiState where

-- Elm standard library
import List

-- local source
import CommonState as Common
import CommonState (CommonState, CommonAction)

{-| The entire application state including gui components -}
type alias GuiState =
    { activeTab   : Tab
    , iconClicked : Int
    , devEnabled  : Bool
    , common : CommonState
    }

{-| Updating the gui state with a common state completely replaces the
    existing background state -}
fromCommonState : CommonState -> GuiState -> GuiState
fromCommonState com gui = { gui | common <- com }

{-| The initial state -}
default : GuiState
default =
    { activeTab   = Log
    , iconClicked = 0
    , devEnabled  = False
    , common = Common.default
    }

type Tab = Log | Settings | Manage | Developer

{-| All actions that can be performed to change state -}
type Action = ChangeTab Tab
            | ClickIcon
            | CommonAction CommonAction
            | NoOp

{-| Transform the state to a new state according to an action -}
update : Action -> GuiState -> GuiState
update action s =
    case action of
        -- clicking the icon 7 times toggles developer tab visibility
        (ChangeTab t) -> {s | activeTab   <- t}
        ClickIcon     -> if s.iconClicked >= 6
                         then { s | iconClicked <- 0
                                  , devEnabled  <- not s.devEnabled
                                  , activeTab   <-
                                     if s.activeTab == Developer
                                         && s.devEnabled
                                     then Log else s.activeTab
                              }
                         else {s | iconClicked <- s.iconClicked + 1}
        (CommonAction a) -> fromCommonState (Common.update a s.common) s
        NoOp          -> s

