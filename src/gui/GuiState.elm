module GuiState where

-- Elm standard library
import List

-- local source
import CommonState as Common
import CommonState (CommonState, CommonAction)

type Tab = Log | Settings | Manage | Developer

{-| The entire application state including gui components -}
type alias GuiState =
    { activeTab   : Tab
    , iconClicked : Int
    , devEnabled  : Bool
    , common : CommonState
    }

{-| All actions that can be performed to change state -}
type Action = ChangeTab Tab
            | ClickIcon
            | CommonAction CommonAction
            | NoOp

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

disabledTabs : Common.ConnectState -> List Tab
disabledTabs s =
    case s of
        Common.Connected    -> []
        Common.NotConnected -> [Settings, Manage, Developer]
        Common.NoCard       -> [Settings, Manage]
        Common.NoPin        -> [Settings, Manage]

{-| Transform the state to a new state according to an action -}
update : Action -> GuiState -> GuiState
update action s =
    let updateCommon a = Common.update a s.common
    in case action of
        (ChangeTab t) -> {s | activeTab <- t}
        -- clicking the icon 7 times toggles developer tab visibility
        ClickIcon     -> if s.iconClicked >= 6
                         then { s | iconClicked <- 0
                                  , devEnabled  <- not s.devEnabled
                                  , activeTab   <-
                                     if s.activeTab == Developer
                                         && s.devEnabled
                                     then Log else s.activeTab
                              }
                         else {s | iconClicked <- s.iconClicked + 1}
        -- An action on the common state can have an affect on the gui-only
        -- state as well. The activeTab may become disabled due to setting the
        -- connected state for instance.
        (CommonAction a) -> case a of
                            (Common.SetConnected c) ->
                                { s | activeTab <-
                                        if s.activeTab `List.member` (disabledTabs c)
                                        then Log else s.activeTab
                                    , common <- updateCommon a
                                }
                            _ -> {s | common <- updateCommon a}
        NoOp -> s

apply : List Action -> GuiState -> GuiState
apply actions state = List.foldr update state actions
