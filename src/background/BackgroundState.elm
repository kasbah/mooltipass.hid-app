module BackgroundState where

-- Elm standard library
import Maybe
import List (..)

-- local source
import CommonState as Common
import CommonState (..)
import DevicePacket (..)
import DeviceFlash (..)
import Byte (..)

type alias BackgroundState = { deviceConnected  : Bool
                             , deviceVersion    : Maybe MpVersion
                             , waitingForDevice : Bool
                             , currentContext   : ByteString
                             , extAwaitingPing  : Bool
                             , extRequest       : ExtensionRequest
                             , mediaImport      : MediaImport
                             , memoryManage     : MemManageState
                             , common           : CommonState
                             }

default : BackgroundState
default = { deviceConnected  = False
          , deviceVersion    = Nothing
          , waitingForDevice = False
          , currentContext   = ""
          , extAwaitingPing  = False
          , extRequest       = NoRequest
          , mediaImport      = NoMediaImport
          , memoryManage     = NotManaging
          , common           = Common.default
          }

type MemManageState =
      NotManaging
    | MemManageRequested
    | MemManageEnd
    | MemManageWaiting
    | MemManageDenied
    | MemManageRead
        (ParentNode, FlashAddress, FlashAddress)
        ByteArray
    | MemManageReadWaiting
        (ParentNode, FlashAddress, FlashAddress)
        ByteArray
    | MemManageReadFav
        (ParentNode, List FlashFavorite)
    | MemManageReadFavWaiting
        (ParentNode, List FlashFavorite)
    | MemManageReadSuccess
        (ParentNode, List Favorite)
    | MemManageWrite
        (List OutgoingPacket)
    | MemManageWriteWaiting
        (List OutgoingPacket)
    | MemManageWriteSuccess
    | MemManageError  String

memoryManageBusy : MemManageState -> Bool
memoryManageBusy mms = case mms of
    NotManaging            -> False
    MemManageError _       -> False
    MemManageDenied        -> False
    MemManageWriteSuccess  -> False
    MemManageReadSuccess _ -> False
    _                      -> True

memoryManaging : MemManageState -> Bool
memoryManaging mms = case mms of
    NotManaging      -> False
    MemManageError _ -> False
    MemManageDenied  -> False
    _                -> True

memManageToInfo : MemManageState -> MemInfo
memManageToInfo mm = case mm of
    NotManaging             -> NoMemInfo
    MemManageRequested      -> MemInfoWaitingForUser
    MemManageWaiting        -> MemInfoWaitingForUser
    MemManageDenied         -> NoMemInfo
    MemManageEnd            -> NoMemInfo
    MemManageRead _ _       -> MemInfoWaitingForDevice
    MemManageReadWaiting _ _ -> MemInfoWaitingForDevice
    MemManageReadFav _                 -> MemInfoWaitingForDevice
    MemManageReadFavWaiting _          -> MemInfoWaitingForDevice
    MemManageReadSuccess (pnode, favs) -> MemInfo {credentials = toCreds pnode
                                                  , favorites  = favs}
    MemManageWrite        _ -> MemInfoWaitingForDevice
    MemManageWriteWaiting _ -> MemInfoWaitingForDevice
    MemManageWriteSuccess   -> MemInfoWaitingForDevice
    MemManageError  _       -> NoMemInfo

type MediaImport =
      NoMediaImport
    | MediaImportRequested    FileId
    | MediaImportStart        (List OutgoingPacket)
    | MediaImportStartWaiting (List OutgoingPacket)
    | MediaImport             (List OutgoingPacket)
    | MediaImportWaiting      (List OutgoingPacket)
    | MediaImportError        String
    | MediaImportSuccess

mediaImportActive : BackgroundState -> Bool
mediaImportActive s = case s.mediaImport of
    NoMediaImport             -> False
    MediaImportError _        -> False
    MediaImportSuccess        -> False
    _                         -> True

type ExtensionRequest =
      ExtWantsCredentials     { context : ByteString }

    | ExtNeedsLogin           { context : ByteString }

    | ExtNeedsPassword        { context : ByteString
                              , login   : ByteString
                              }

    | ExtCredentials          { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNoCredentials

    | ExtWantsToWrite         { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNeedsNewContext      { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNeedsToWritePassword { context  : ByteString
                              , password : ByteString
                              }

    | ExtWriteComplete        { context  : ByteString }

    | ExtNotWritten

    | NoRequest

outgoingExtRequestToLog : ExtensionRequest -> Maybe String
outgoingExtRequestToLog r = case r of
    ExtNeedsLogin {context} ->
        Just <| "> requesting credentials for " ++ context
    ExtWantsToWrite {context, login, password} ->
        Just <| "> requesting to write credentials for " ++ context
    ExtNeedsNewContext {context, login, password} ->
        Just <| "> adding new service: " ++ context
    _ -> Nothing

incomingExtRequestToLog : ExtensionRequest -> Maybe String
incomingExtRequestToLog r = case r of
    ExtNoCredentials    -> Just "access denied or no credentials"
    ExtNotWritten       -> Just "access denied"
    ExtWriteComplete _  -> Just "credentials written"
    ExtCredentials   _  -> Just "credentials retrieved"
    _ -> Nothing

type BackgroundAction = SetHidConnected     Bool
                      | SetWaitingForDevice Bool
                      | SetExtAwaitingPing  Bool
                      | SetExtRequest       ExtensionRequest
                      | SetMediaImport      MediaImport
                      | SetMemManage        MemManageState
                      | Interpret           ReceivedPacket
                      | CommonAction        CommonAction
                      | NoOp

apply : List BackgroundAction -> BackgroundState -> BackgroundState
apply actions state = foldr update state actions

update : BackgroundAction -> BackgroundState -> BackgroundState
update action s =
    let updateCommon a = Common.update a s.common
    in case action of
        SetHidConnected b ->
            if not b
            then apply
               [ CommonAction (SetDeviceStatus NotConnected)
               , if mediaImportActive s
                 then SetMediaImport (MediaImportError "device disconnected")
                 else NoOp
               , if memoryManaging s.memoryManage
                 then SetMemManage NotManaging
                 else NoOp
               ]
               {s | deviceConnected <-  False}
            else {s | deviceConnected <- True}
        SetExtAwaitingPing b -> {s | extAwaitingPing <- b}
        SetExtRequest d -> {s | extRequest <- d}
        SetMediaImport t -> setMedia t s
        SetWaitingForDevice b -> {s | waitingForDevice <- b}
        SetMemManage m -> setMemManage m s
        CommonAction StartMemManage    -> setMemManage MemManageRequested s
        CommonAction EndMemManage      -> setMemManage MemManageEnd s
        CommonAction (SaveMemManage d) -> case s.memoryManage of
            MemManageReadSuccess (pNode, _) ->
                setMemManage (MemManageWrite (fromFavs d.favorites pNode ++ fromCreds d.credentials pNode)) s
            _ -> s
        CommonAction (SetDeviceStatus c) ->
            let s' = {s | common <- updateCommon (SetDeviceStatus c)}
            in if c /= s.common.deviceStatus
               then update
                        ( if
                            | mediaImportActive s && (c == Locked || c == NotConnected) ->
                                SetMediaImport (MediaImportError "interrupted by device")
                            | memoryManaging s.memoryManage && c == Locked ->
                                SetMemManage NotManaging
                            | otherwise -> NoOp )
                    {s' | common <-
                            Common.update
                                (AppendToLog (connectToLog c))
                                s'.common
                        , currentContext <- ""
                        , deviceVersion  <- if c == NotConnected
                                            then Nothing
                                            else s.deviceVersion
                    }
               else s
        CommonAction (StartImportMedia p) ->
            if not (mediaImportActive s)
            then setMedia (MediaImportRequested p) s
            else s
        CommonAction a -> {s | common <- updateCommon a}
        Interpret p -> interpret p s
        NoOp -> s


interpret : ReceivedPacket -> BackgroundState -> BackgroundState
interpret packet s =
    let setExtRequest r = case incomingExtRequestToLog r of
        Just str -> appendToLog str {s | extRequest <- r}
        Nothing -> {s | extRequest <- r}
    in case packet of
        ReceivedGetLogin ml -> case ml of
            Just l ->
                case s.extRequest of
                ExtNeedsLogin c ->
                    setExtRequest (ExtNeedsPassword {c | login = l})
                _ -> setExtRequest NoRequest
            Nothing -> setExtRequest ExtNoCredentials
        ReceivedGetPassword mp -> case mp of
            Just p ->
                case s.extRequest of
                ExtNeedsPassword c ->
                    setExtRequest (ExtCredentials {c | password = p})
                _ -> setExtRequest NoRequest
            Nothing -> setExtRequest ExtNoCredentials
        ReceivedSetLogin r ->
            case s.extRequest of
                 ExtWantsToWrite c ->
                     if r == Done
                     then setExtRequest (ExtNeedsToWritePassword { c - login })
                     else setExtRequest ExtNotWritten
                 _ -> setExtRequest NoRequest
        ReceivedSetPassword r ->
            case s.extRequest of
                 ExtNeedsToWritePassword c ->
                     if r == Done
                     then setExtRequest (ExtWriteComplete { c - password })
                     else setExtRequest ExtNotWritten
                 _ -> setExtRequest NoRequest
        ReceivedSetContext r ->
            case r of
                ContextSet -> case s.extRequest of
                    ExtWantsCredentials c ->
                        {s | currentContext <- c.context
                           , extRequest <- ExtNeedsLogin c}
                    ExtWantsToWrite c ->
                        {s | currentContext <- c.context}
                    ExtNeedsPassword c  ->
                        {s | currentContext <- c.context}
                    ExtNeedsToWritePassword c ->
                        {s | currentContext <- c.context}
                    -- this fall-through would be: we have no idea what
                    -- context we set so we just keep the original state
                    _ -> s
                UnknownContext -> case s.extRequest of
                    ExtWantsToWrite c ->
                        {s | extRequest <- ExtNeedsNewContext c}
                    ExtWantsCredentials _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsPassword _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsToWritePassword _ ->
                        {s | extRequest <- ExtNotWritten}
                    _ -> s
                NoCardForContext ->
                    update (CommonAction (SetDeviceStatus NoCard)) s
        ReceivedAddContext r ->
            case s.extRequest of
                 ExtNeedsNewContext c ->
                     if r == Done
                     then setExtRequest (ExtWantsToWrite c)
                     else setExtRequest ExtNotWritten
                 _ -> setExtRequest NoRequest
        ReceivedGetVersion v ->
                appendToLog
                    ("device is "
                        ++ v.version ++ " "
                        ++ toString v.flashMemSize
                        ++ "MBit")
                {s | deviceVersion <- Just v}
        ReceivedImportMediaStart r ->
            case s.mediaImport of
                MediaImportStartWaiting ps ->
                    if r == Done
                    then setMedia (MediaImport ps) s
                    else setMedia (MediaImportError "Import start failed") s
                _ -> setMedia (MediaImportError (unexpected "ImportMediaStart")) s
        ReceivedImportMedia r ->
            case s.mediaImport of
                MediaImportWaiting (p::ps) ->
                    if r == Done
                    then setMedia (MediaImport ps) s
                    else setMedia (MediaImportError "Import write failed") s
                _ -> setMedia (MediaImportError (unexpected "ImportMedia")) s
        ReceivedImportMediaEnd r ->
            case s.mediaImport of
                MediaImportWaiting [] ->
                    if r == Done
                    then setMedia MediaImportSuccess s
                    else setMedia (MediaImportError "Import end-write failed") s
                _ -> setMedia (MediaImportError (unexpected "ImportMediaEnd")) s
        ReceivedManageModeStart r ->
            if r == Done
            then setMemManage (MemManageRead (EmptyParentNode, null, null) [])
                    (update (CommonAction (SetDeviceStatus ManageMode)) s)
            else setMemManage MemManageDenied
                    (update (CommonAction (SetDeviceStatus Unlocked)) s)
        ReceivedGetStartingParent a -> case s.memoryManage of
            MemManageReadWaiting (EmptyParentNode,null,null) [] ->
                if a /= null then
                    setMemManage (MemManageRead (EmptyParentNode, a, null) []) s
                else
                    setMemManage (MemManageReadSuccess (EmptyParentNode, emptyFavorites)) s
            _ -> setMemManage (MemManageError (unexpected "starting parent")) s
        ReceivedReadFlashNode ba ->
            case s.memoryManage of
                MemManageReadWaiting d prevBa ->
                    if length (prevBa ++ ba) == nodeSize
                        then case parse d (prevBa ++ ba) of
                            Ok d'  -> setMemManage (MemManageRead d' []) s
                            Err err -> setMemManage (MemManageError err) s
                        else setMemManage (MemManageReadWaiting d (prevBa ++ ba)) s
                _ -> setMemManage (MemManageError (unexpected "flash node")) s
        ReceivedGetFavorite (p,c) ->
            case s.memoryManage of
                MemManageReadFavWaiting (n,ffavs) ->
                    let ffavs' = ({parentNode = p, childNode = c})::ffavs
                    in if length ffavs' == maxFavs then
                          setMemManage (MemManageReadSuccess (n, toFavs ffavs' n)) s
                       else
                          setMemManage (MemManageReadFav (n, ffavs')) s
                _ -> setMemManage (MemManageError (unexpected "favorite")) s
        ReceivedManageModeEnd r ->
            if r == Done
            then update (CommonAction (SetDeviceStatus Unlocked)) s
            else setMemManage (MemManageError "device did not exit mem-manage when asked") s
        ReceivedSetFavorite r -> case s.memoryManage of
            MemManageWriteWaiting (p::ps) ->
                if r == Done
                then setMemManage (MemManageWrite ps) s
                else setMemManage (MemManageError "write favorite denied") s
            MemManageWriteWaiting [] ->
                if r == Done
                then setMemManage (MemManageRead (EmptyParentNode, null, null) []) s
                else setMemManage (MemManageError "write favorite denied") s
            _ -> setMemManage (MemManageError (unexpected "set favorite")) s
        ReceivedWriteFlashNode r -> case s.memoryManage of
            MemManageWriteWaiting (p::ps) ->
                if r == Done
                then setMemManage (MemManageWrite ps) s
                else setMemManage (MemManageError "write node denied") s
            MemManageWriteWaiting [] ->
                if r == Done
                then setMemManage (MemManageRead (EmptyParentNode, null, null) []) s
                else setMemManage (MemManageError "write node denied") s
            _ -> setMemManage (MemManageError (unexpected "write node")) s
        x -> appendToLog
                ("Error: received unhandled packet " ++ toString x)
                s

setMemManage : MemManageState -> BackgroundState -> BackgroundState
setMemManage m s =
    let s' = setInfo (memManageToInfo m)
        setInfo i = {s | common <- updateCommon (SetMemInfo i )}
        setManage m' = { s' | memoryManage <- m'}
        updateCommon a = Common.update a s.common
    in case m of
        MemManageError str -> appendToLog ("Mem Manage Error: " ++ str) (setManage m)
        MemManageRequested ->
            if not (memoryManaging s.memoryManage)
            then
                setManage MemManageRequested
            else
                setManage
                    (MemManageError
                        "Manage mode requested while aready in manage mode")
        MemManageEnd ->
            if memoryManaging s.memoryManage
            then
                setManage MemManageEnd
            else
                setManage
                    (MemManageError
                        "Manage mode end requested while aready in manage mode")
        MemManageDenied ->
            if s.memoryManage == MemManageWaiting
            then setManage MemManageDenied
            else setManage <| MemManageError (unexpected "memory manage denied")
        _ -> setManage m

setMedia : MediaImport -> BackgroundState -> BackgroundState
setMedia imp s =
    let c             = s.common
        s'            = {s | mediaImport <- imp}
        updateInfo i =
            {s' | common <- updateCommon (SetImportInfo i)}
        updateCommon a = Common.update a s.common
    in case imp of
        MediaImportError str    -> updateInfo (ImportError str)
        MediaImportRequested id -> updateInfo (ImportRequested id)
        MediaImportStart ps     -> case s.common.importInfo of
            ImportRequested id ->
                updateInfo (Importing id (length ps) (length ps))
            _ -> updateInfo
                (ImportError (unexpected "MediaImportStart"))
        MediaImport ps          -> case s.common.importInfo of
            Importing id _ total   ->
                updateInfo (Importing id (length ps) total)
            _ -> updateInfo
                    (ImportError (unexpected "MediaImport"))
        MediaImportSuccess      -> case s.common.importInfo of
            Importing id _ _
                -> updateInfo (Imported id)
            _ -> updateInfo
                    (ImportError (unexpected "MediaImportSuccess"))
        _ -> s'


fromResult :  Result Error ReceivedPacket -> BackgroundAction
fromResult r = case r of
    Err err -> appendToLog' ("HID Error: " ++ err)
    Ok p    -> Interpret p


appendToLog' str = CommonAction (AppendToLog str)
appendToLog str state = update (appendToLog' str) state

unexpected str = "Received unexpected " ++ str ++  " from device"
