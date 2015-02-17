module GuiState where

-- Elm standard library
import List (..)
import Maybe

-- local source
import CommonState as Common
import CommonState (..)
import Util (..)

type Tab = Log | Settings | Manage | Developer

type ImportRequest =
      Requested
    | Waiting
    | RequestFile FileId
    | NotRequested

{-| The entire GUI state -}
type alias GuiState =
    { activeTab   : Tab
    , iconClicked : Int
    , devEnabled  : Bool
    , importMedia : ImportRequest
    , unsavedMemInfo : Maybe MemoryInfo
    , common      : CommonState
    }

{-| All actions that can be performed to change GUI state directly -}
type Action = ChangeTab Tab
            | ClickIcon
            | SetImportMedia ImportRequest
            | CommonAction CommonAction
            | AddFav (String, String)
            | RemoveFav (String, String)
            | MoveFavUp (String, String)
            | MoveFavDown (String, String)
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
                                        | not s.devEnabled && not (Developer `member` disabledTabs s.common.connected) -> Developer
                                        | otherwise -> s.activeTab
                              }
                         else {s | iconClicked <- s.iconClicked + 1}
        SetImportMedia r   -> case r of
            RequestFile p -> if s.importMedia == Waiting
                             then {s | importMedia <- r}
                             else s
            _ -> {s | importMedia <- r}
        AddFav f        ->
            {s | unsavedMemInfo <- Maybe.map (addToFavs f) s.unsavedMemInfo}
        RemoveFav f   ->
            {s | unsavedMemInfo <- Maybe.map (removeFromFavs f) s.unsavedMemInfo}
        MoveFavUp f   ->
            {s | unsavedMemInfo <- Maybe.map (moveFavUp f) s.unsavedMemInfo}
        MoveFavDown f   ->
            {s | unsavedMemInfo <- Maybe.map (moveFavDown f) s.unsavedMemInfo}
        -- An action on the common state can have an affect on the gui-only
        -- state as well. The activeTab may become disabled due to setting the
        -- connected state for instance.
        (CommonAction a) -> case a of
                            (SetConnected c) ->
                                { s | activeTab <-
                                        if s.activeTab `member` (disabledTabs c)
                                        then Log else s.activeTab
                                    , common <- updateCommon a
                                }
                            (SetMemoryInfo i) ->
                                if s.unsavedMemInfo == Nothing
                                   || i /= s.common.memoryInfo
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
addToFavs f info = {info | favorites <- replaceFirst Nothing (Just f) info.favorites}

moveFavUp : (String, String) -> MemoryInfo -> MemoryInfo
moveFavUp f info =
    {info | favorites <-
        reverse <| foldl (switchFav f) [] info.favorites
    }

moveFavDown : (String, String) -> MemoryInfo -> MemoryInfo
moveFavDown f info =
    {info | favorites <-
        foldr (switchFav f) [] info.favorites
    }

switchFav f x zs = if | zs == []   -> [x]
                   | x == (Just f) -> head zs::x::(tail zs)
                   | otherwise     -> x::head zs::(tail zs)

{-| Apply 'update' to a list of actions -}
apply : List Action -> GuiState -> GuiState
apply actions state = foldr update state actions
