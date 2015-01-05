module State where

import Signal

type Tab = Log | Settings | Manage | Developer
type ConnectState = NotConnected | Connected | NoCard | NoPin

{-| The entire application state -}
type alias State =
    { connect     : ConnectState
    , activeTab   : Tab
    , iconClicked : Int
    , devEnabled  : Bool
    , log         : String
    }

defaultState : State
defaultState =
    { connect     = Connected
    , activeTab   = Log
    , iconClicked = 0
    , devEnabled  = True
    , log         = "connecting ..."
    }

{-| All actions that can be performed to change state -}
type Action = AppendToLog String
            | ChangeTab Tab
            | ClearLog
            | ClickIcon
            | NoOp
            | SetConnected ConnectState

{-| Transform the state to a new state according to an action -}
update : Action -> State -> State
update action s =
    case action of
        (ChangeTab t)    -> {s | activeTab   <- t}
        (SetConnected c) -> {s | connect     <- c}
        ClearLog         -> {s | log         <- ""}
        -- clicking the icon 7 times toggles developer tab visibility
        ClickIcon        -> if s.iconClicked >= 6
                            then { s | iconClicked <- 0
                                     , devEnabled <- not s.devEnabled
                                     , activeTab <- if s.activeTab == Developer
                                                        && s.devEnabled
                                                    then Log else s.activeTab
                                 }

                            else {s | iconClicked <- s.iconClicked + 1}
        (AppendToLog str) -> {s | log <- s.log ++ str}
        NoOp             -> s

actions : Signal.Channel Action
actions = Signal.channel NoOp
