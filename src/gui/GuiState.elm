module GuiState where

-- Elm standard library
import List
import List (..)
import Maybe

-- local source
import CommonState as Common
import CommonState (..)
import Util (..)

type Tab = Log | Settings | Manage | Developer

type TransferRequest =
      Requested
    | Waiting
    | RequestFile FileId
    | NotRequested

{-| The entire GUI state -}
type alias GuiState =
    { activeTab   : Tab
    , iconClicked : Int
    , devEnabled  : Bool
    , importMedia : TransferRequest
    , unsavedMemInfo : Maybe MemoryInfo
    , common      : CommonState
    }

{-| All actions that can be performed to change GUI state directly -}
type Action = ChangeTab Tab
            | ClickIcon
            | SetImportMedia TransferRequest
            | CommonAction CommonAction
            | AddToFavs (String, String)
            | RemoveFromFavs (String, String)
            | NoOp

{-| The initial state -}
default : GuiState
default =
    { activeTab   = Log
    , iconClicked = 0
    , devEnabled  = False
    , importMedia = NotRequested
    , unsavedMemInfo = Nothing
    , common      = Common.default
    }

{-| The non-visible tabs according to the 'CommonState.ConnectState' -}
disabledTabs : Common.ConnectState -> List Tab
disabledTabs s =
    case s of
        Common.Connected    -> []
        Common.NotConnected -> [Settings, Manage, Developer]
        Common.NoCard       -> [Settings, Manage]
        Common.NoPin        -> [Settings, Manage, Developer]

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
                                     if | s.activeTab == Developer && s.devEnabled -> Log
                                        | not s.devEnabled && not (Developer `List.member` disabledTabs s.common.connected) -> Developer
                                        | otherwise -> s.activeTab
                              }
                         else {s | iconClicked <- s.iconClicked + 1}
        SetImportMedia r   -> case r of
            RequestFile p -> if s.importMedia == Waiting
                             then {s | importMedia <- r}
                             else s
            _ -> {s | importMedia <- r}
        AddToFavs f        ->
            {s | unsavedMemInfo <- Maybe.map (addToFavs f) s.unsavedMemInfo}
        RemoveFromFavs f   ->
            {s | unsavedMemInfo <- Maybe.map (removeFromFavs f) s.unsavedMemInfo}
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
                            (Common.SetMemoryInfo i) ->
                                if s.unsavedMemInfo == Nothing || i /= s.common.memoryInfo
                                then {s | unsavedMemInfo <- Just i
                                        , common <- updateCommon a
                                     }
                                else s
                            _ -> {s | common <- updateCommon a}
        NoOp -> s

removeFromFavs : (String, String) -> MemoryInfo -> MemoryInfo
removeFromFavs f info =
    {info | favorites <-
        map (\x -> if x == (Just f) then Nothing else x) info.favorites
    }

addToFavs : (String, String) -> MemoryInfo -> MemoryInfo
addToFavs f info = {info | favorites <- replace info.favorites Nothing (Just f)}

{-| Apply 'update' to a list of actions -}
apply : List Action -> GuiState -> GuiState
apply actions state = List.foldr update state actions
