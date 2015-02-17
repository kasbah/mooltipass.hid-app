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
    { activeTab      : Tab
    , iconClicked    : Int
    , devEnabled     : Bool
    , importMedia    : ImportRequest
    , unsavedMemInfo : MemoryInfo
    , common         : CommonState
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
    { activeTab      = Log
    , iconClicked    = 0
    , devEnabled     = False
    , importMedia    = NotRequested
    , unsavedMemInfo = NoMemoryInfo
    , common         = Common.default
    }

{-| The non-visible tabs according to the 'CommonState.DeviceStatus' -}
disabledTabs : Common.DeviceStatus -> List Tab
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
        errorTryingTo str =
            appendToLog
                ("Error: trying to " ++ str ++ " favorite without having memory data")
                s
    in case action of
        (ChangeTab t) -> if t == Manage && s.unsavedMemInfo == NoMemoryInfo
                         then update
                                (CommonAction StartMemManage)
                                {s | activeTab <- Manage}
                         else {s | activeTab <- t}
        -- clicking the icon 7 times toggles developer tab visibility
        ClickIcon     -> if s.iconClicked >= 6
                         then { s | iconClicked <- 0
                                  , devEnabled  <- not s.devEnabled
                                  , activeTab   <-
                                     if | s.activeTab == Developer
                                            && s.devEnabled
                                                    -> Log
                                        | not s.devEnabled
                                            && not (Developer
                                                `member`
                                                    disabledTabs s.common.connected)
                                                    -> Developer
                                        | otherwise -> s.activeTab
                              }
                         else {s | iconClicked <- s.iconClicked + 1}
        SetImportMedia r   -> case r of
            RequestFile p -> if s.importMedia == Waiting
                             then {s | importMedia <- r}
                             else s
            _ -> {s | importMedia <- r}
        AddFav f        ->
            case s.unsavedMemInfo of
                MemoryData d -> {s | unsavedMemInfo <- addToFavs f d}
                _ -> errorTryingTo "add"
        RemoveFav f   ->
            case s.unsavedMemInfo of
                MemoryData d -> {s | unsavedMemInfo <- removeFromFavs f d}
                _ -> errorTryingTo "remove"
        MoveFavUp f   ->
            case s.unsavedMemInfo of
                MemoryData d -> {s | unsavedMemInfo <- moveFavUp f d}
                _ -> errorTryingTo "move"
        MoveFavDown f   ->
            case s.unsavedMemInfo of
                MemoryData d -> {s | unsavedMemInfo <- moveFavDown f d}
                _ -> errorTryingTo "move"
        -- An action on the common state can have an affect on the gui-only
        -- state as well. The activeTab may become disabled due to setting the
        -- device state for instance.
        CommonAction a -> case a of
                            SetConnected c ->
                                { s | activeTab <-
                                        if s.activeTab `member` (disabledTabs c)
                                        then Log else s.activeTab
                                    , common <- updateCommon a
                                }
                            SetMemoryInfo i ->
                                case i of
                                    (MemoryData d) -> case s.common.memoryInfo of
                                        (MemoryData cd) ->
                                            if d /= cd
                                            then {s | unsavedMemInfo <- i
                                                    , common <- updateCommon a
                                                 }
                                            else s
                                        _ -> {s | unsavedMemInfo <- i
                                                , common <- updateCommon a
                                             }
                                    _ -> {s | unsavedMemInfo <- i
                                            , common <- updateCommon a
                                         }
                            StartMemManage ->
                                {s | unsavedMemInfo <- MemManageRequested
                                   , common <- updateCommon a
                                }
                            _ -> {s | common <- updateCommon a}
        NoOp -> s

removeFromFavs : (String, String) -> MemoryInfoData -> MemoryInfo
removeFromFavs f info =
    MemoryData
    {info | favorites <-
        map (\x -> if x == (Just f) then Nothing else x) info.favorites
    }

addToFavs : (String, String) -> MemoryInfoData -> MemoryInfo
addToFavs f info =
    MemoryData
    {info | favorites <- replaceFirst Nothing (Just f) info.favorites}

moveFavUp : (String, String) -> MemoryInfoData -> MemoryInfo
moveFavUp f info =
    MemoryData
    {info | favorites <-
        reverse <| foldl (switchFav f) [] info.favorites
    }

moveFavDown : (String, String) -> MemoryInfoData -> MemoryInfo
moveFavDown f info =
    MemoryData
    {info | favorites <-
        foldr (switchFav f) [] info.favorites
    }

switchFav f x zs = if | zs == []   -> [x]
                   | x == (Just f) -> head zs::x::(tail zs)
                   | otherwise     -> x::head zs::(tail zs)

{-| Apply 'update' to a list of actions -}
apply : List Action -> GuiState -> GuiState
apply actions state = foldr update state actions

appendToLog' str = CommonAction (AppendToLog str)
appendToLog str state = update (appendToLog' str) state
