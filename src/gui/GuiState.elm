module GuiState where

-- Elm standard library
import List (..)
import Maybe

-- local source
import CommonState as Common
import CommonState (..)
import Util (..)
import Byte (..)

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
    , writeMem       : ImportRequest
    , unsavedMemInfo : MemInfo
    , common         : CommonState
    }

{-| All actions that can be performed to change GUI state directly -}
type Action = ChangeTab Tab
            | ClickIcon
            | SetImportMedia ImportRequest
            | SetWriteMem ImportRequest
            | SetUnsavedMem MemInfo
            | CommonAction CommonAction
            | AddFav (FlashAddress, FlashAddress)
            | RemoveFav (FlashAddress, FlashAddress)
            | MoveFavUp (FlashAddress, FlashAddress)
            | MoveFavDown (FlashAddress, FlashAddress)
            | RemoveCred (FlashAddress, FlashAddress)
            | NoOp

{-| The initial state -}
default : GuiState
default =
    { activeTab      = Log
    , iconClicked    = 0
    , devEnabled     = False
    , importMedia    = NotRequested
    , writeMem       = NotRequested
    , unsavedMemInfo = NoMemInfo
    , common         = Common.default
    }

{-| The non-selectable tabs according to the 'CommonState.DeviceStatus' -}
disabledTabs : Common.DeviceStatus -> List Tab
disabledTabs s =
    case s of
        Common.Unlocked     -> []
        Common.NotConnected -> [Settings, Manage, Developer]
        Common.NoCard       -> [Settings, Manage]
        Common.Locked       -> [Settings, Manage, Developer]
        Common.ManageMode   -> [Settings]

{-| Transform the state to a new state according to an action -}
update : Action -> GuiState -> GuiState
update action s =
    let updateCommon a = Common.update a s.common
        errorTryingTo str =
            appendToLog
                ("Error: trying to " ++ str ++ " without having memory data")
                s
    in case action of
        ChangeTab t -> if t == Manage && s.unsavedMemInfo == NoMemInfo
                         then {s | activeTab <- Manage
                                 , unsavedMemInfo <- MemInfoRequest
                              }
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
                                            && not (Developer `member`
                                                    disabledTabs s.common.deviceStatus)
                                                    -> Developer
                                        | otherwise -> s.activeTab
                              }
                         else {s | iconClicked <- s.iconClicked + 1}
        SetImportMedia r   -> case r of
            RequestFile p -> if s.importMedia == Waiting
                             then {s | importMedia <- r}
                             else s
            _ -> {s | importMedia <- r}
        SetWriteMem r   -> case r of
            RequestFile p -> if s.writeMem == Waiting
                             then {s | writeMem <- r}
                             else s
            _ -> {s | writeMem <- r}
        AddFav f        ->
            case s.unsavedMemInfo of
                MemInfo d -> {s | unsavedMemInfo <- addToFavs f d}
                _ -> errorTryingTo "add favorite"
        RemoveFav f   ->
            case s.unsavedMemInfo of
                MemInfo d -> {s | unsavedMemInfo <- removeFromFavs f d}
                _ -> errorTryingTo "remove favorite"
        MoveFavUp f   ->
            case s.unsavedMemInfo of
                MemInfo d -> {s | unsavedMemInfo <- moveFavUp f d}
                _ -> errorTryingTo "move favorite"
        MoveFavDown f   ->
            case s.unsavedMemInfo of
                MemInfo d -> {s | unsavedMemInfo <- moveFavDown f d}
                _ -> errorTryingTo "move favorite"
        RemoveCred c ->
            case s.unsavedMemInfo of
                MemInfo d -> {s | unsavedMemInfo <- removeCred c d}
                _ -> errorTryingTo "remove credential"
        SetUnsavedMem i -> {s | unsavedMemInfo <- i}
        -- An action on the common state can have a effect on the gui-only
        -- state as well. The activeTab may become disabled due to setting the
        -- device state for instance.
        CommonAction a ->
            let s' = {s | common <- updateCommon a}
            in case a of
                SetDeviceStatus c ->
                    { s' | activeTab <-
                            if s.activeTab `member` (disabledTabs c)
                            then Log else s.activeTab
                    }
                SetMemInfo i ->
                    let updateMemInfo = {s' | unsavedMemInfo <- i}
                    in case i of
                        MemInfo d -> case s.common.memoryInfo of
                            MemInfo cd ->
                                if d /= cd
                                then updateMemInfo
                                else s
                            _ -> updateMemInfo
                        _ -> updateMemInfo
                _ -> s'
        NoOp -> s

removeFromFavs : (FlashAddress, FlashAddress) -> MemInfoData -> MemInfo
removeFromFavs f info =
    MemInfo
    {info | favorites <-
        map (\x -> if x == (Just f) then Nothing else x) info.favorites
    }

addToFavs : (FlashAddress, FlashAddress) -> MemInfoData -> MemInfo
addToFavs f info =
    MemInfo
    {info | favorites <- replaceFirst Nothing (Just f) info.favorites}

moveFavUp : (FlashAddress, FlashAddress) -> MemInfoData -> MemInfo
moveFavUp f info =
    MemInfo
    {info | favorites <-
        reverse <| foldl (switchFav f) [] info.favorites
    }

moveFavDown : (FlashAddress, FlashAddress) -> MemInfoData -> MemInfo
moveFavDown f info =
    MemInfo
    {info | favorites <-
        foldr (switchFav f) [] info.favorites
    }

removeCred : (FlashAddress, FlashAddress) -> MemInfoData -> MemInfo
removeCred (addr1,addr2) info =
    removeFromFavs (addr1, addr2)
        {info | credentials <-
                filter (\(s,ls) -> not (isEmpty ls))
                <| map
                    (\(s,ls) ->
                        if s.address == addr1
                        then (s, filter (\{address} -> address /= addr2) ls)
                        else (s,ls))
                    info.credentials
        }

{-| Add logins and a new service or add them to an existing service, returns
    'Nothing' when out of addresses -}
addCreds : Service -> MemInfoData -> Maybe MemInfo
addCreds (service,logins) data =
    let addLogins ls = ls ++ filter (\l -> not (any (\l' -> l'.login == l.login) ls)) newLogins
        add newS newAddrs =
            { data | credentials <- if any (\(s,_) -> s.service == service.service) data.credentials
                                    then replaceService newS
                                    else addService newS
                   , addresses <- newAddrs
             }
        oldService =
            foldr
                (\(s,ls) z -> if s.service == service.service then Just (s,ls) else z)
                Nothing
                data.credentials
        addService newS = sortBy (\(s,_) -> s.service) <| newS::data.credentials
        replaceService newS = foldr
                            (\(s,ls) z -> if s.service == service.service then newS::z else (s,ls)::z)
                            []
                            data.credentials
        newLogins  = map2 (\l addr -> {l | address <- addr}) logins data.addresses
        newService = case oldService of
            Just (s,ls) -> Just ((s, addLogins ls), drop (length (addLogins ls) - length ls) data.addresses)
            Nothing     -> Maybe.map (\s -> ((s, newLogins), drop (1 + length newLogins) data.addresses)) newService'
        newService' =
            Maybe.map (\addr -> {service | address <- addr}) <| maybeHead (drop (length newLogins) data.addresses)
   in case newService of
         Just (newS,newAddrs) -> if length newLogins == length logins
                                 then Just <| MemInfo <| add newS newAddrs
                                 else Nothing
         Nothing              -> Nothing

switchFav f x zs = if | zs == []   -> [x]
                   | x == (Just f) -> head zs::x::(tail zs)
                   | otherwise     -> x::head zs::(tail zs)

{-| Apply 'update' to a list of actions -}
apply : List Action -> GuiState -> GuiState
apply actions state = foldr update state actions

appendToLog' str = CommonAction (AppendToLog str)
appendToLog str state = update (appendToLog' str) state
