module State where

-- Elm standard library
import Signal (..)
import List

{-| The state signal to map our main element to -}
state : Signal Action -> Signal GuiState
state = foldp update default

{-| The entire application state including gui components -}
type alias GuiState =
    { connect     : ConnectState
    , activeTab   : Tab
    , iconClicked : Int
    , devEnabled  : Bool
    , log         : List String
    }

{-| The background state excluding gui components. It is a subset of 'GuiState'.-}
type alias BgState =
    { connect     : ConnectState
    , log         : List String
    }

fromBgState : BgState -> GuiState -> GuiState
fromBgState bg gui = { gui | connect <- bg.connect
                           , log     <- bg.log
                     }

toBgState : GuiState -> BgState
toBgState gui = { connect = gui.connect
                , log     = gui.log
                }

{-| The initial state -}
default : GuiState
default =
    { connect     = NotConnected
    , activeTab   = Log
    , iconClicked = 0
    , devEnabled  = False
    , log         = []
    }

type Tab = Log | Settings | Manage | Developer

type ConnectState = NotConnected | Connected | NoCard | NoPin

{-| All actions that can be performed to change state -}
type Action = SetLog (List String)
            | ChangeTab Tab
            | ClickIcon
            | NoOp
            | SetConnected ConnectState

{-| Transform the state to a new state according to an action -}
update : Action -> GuiState -> GuiState
update action s =
    case action of
        (ChangeTab t)    -> {s | activeTab   <- t}
        (SetConnected c) -> {s | connect     <- c}
        -- clicking the icon 7 times toggles developer tab visibility
        ClickIcon        -> if s.iconClicked >= 6
                            then { s | iconClicked <- 0
                                     , devEnabled  <- not s.devEnabled
                                     , activeTab   <-
                                        if s.activeTab == Developer
                                            && s.devEnabled
                                        then Log else s.activeTab
                                 }
                            else {s | iconClicked <- s.iconClicked + 1}
        (SetLog l)      -> {s | log <- l}
        NoOp            -> s

