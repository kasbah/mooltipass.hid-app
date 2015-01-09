module State where

-- Elm standard library
import Signal (..)
import List

{-| The state signal to map our main element to -}
state : Signal Action -> Signal GuiState
state = foldp update default

{-| The entire application state including gui components -}
type alias GuiState =
    { activeTab   : Tab
    , iconClicked : Int
    , devEnabled  : Bool
    , bgState     : BgState
    }

{-| The background state excluding gui components -}
type alias BgState =
    { connect     : ConnectState
    , log         : List String
    }

fromBgState : BgState -> GuiState -> GuiState
fromBgState bg gui = { gui | bgState <- bg }

toBgState : GuiState -> BgState
toBgState gui = gui.bgState

{-| The initial state -}
default : GuiState
default =
    { activeTab   = Log
    , iconClicked = 0
    , devEnabled  = False
    , bgState     = defaultBgState
    }

defaultBgState : BgState
defaultBgState =
    { connect     = NotConnected
    , log         = []
    }

type Tab = Log | Settings | Manage | Developer

type ConnectState = NotConnected | Connected | NoCard | NoPin

{-| All actions that can be performed to change state -}
type Action = SetLog (List String)
            | ChangeTab Tab
            | ClickIcon
            | NoOp

{-| Transform the state to a new state according to an action -}
update : Action -> GuiState -> GuiState
update action s =
    let bgState' = s.bgState
    in case action of
        (ChangeTab t)    -> {s | activeTab   <- t}
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
        (SetLog l)      ->{s | bgState <-  {bgState' | log <- l} }
        NoOp            -> s

