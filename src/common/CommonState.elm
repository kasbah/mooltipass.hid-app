module CommonState where

-- Elm standard library
import List (..)
import String

-- local source
import Byte (..)

{-| The background state excluding gui components -}
type alias CommonState =
    { deviceStatus : DeviceStatus
    , log          : List String
    , importInfo   : ImportInfo
    , memoryInfo   : MemInfo
    , forceUpdate  : Bool
    }

default : CommonState
default =
    { deviceStatus = NotConnected
    , log          = []
    , importInfo   = NoImport
    , memoryInfo   = NoMemInfo
    , forceUpdate  = True
    }

type alias Favorite = Maybe (String, String)

maxFavs = 15
emptyFavorites = [Nothing,Nothing,Nothing,Nothing,Nothing
                 ,Nothing,Nothing,Nothing,Nothing,Nothing
                 ,Nothing,Nothing,Nothing,Nothing,Nothing]

type MemInfo =
      MemInfo MemInfoData
    | MemInfoRequest
    | MemInfoSave MemInfoData
    | MemInfoWaitingForUser
    | MemInfoWaitingForDevice
    | NoMemInfo

type alias Service = ((String, FlashAddress), List (String, FlashAddress))

type alias MemInfoData =
    { credentials : List Service
    , favorites   : List Favorite
    }

type DeviceStatus = NotConnected | Unlocked | NoCard | Locked

type ImportInfo =
      ImportRequested FileId
    | Importing FileId Int Int
    | Imported FileId
    | ImportError String
    | NoImport

type alias FileId = String

fileName : FileId -> String
fileName id = case String.split ":" id of
    [_,name] -> name
    _ -> ""

connectToLog : DeviceStatus -> String
connectToLog c = case c of
    NotConnected -> "device disconnected"
    Unlocked     -> "device status: unlocked"
    NoCard       -> "device status: no card present"
    Locked       -> "device status: locked"

{-| All actions that can be performed to change the common state -}
type CommonAction = SetLog (List String)
                  | SetDeviceStatus DeviceStatus
                  | AppendToLog String
                  | GetState
                  | SetImportInfo ImportInfo
                  | StartImportMedia FileId
                  | SetMemInfo MemInfo
                  | StartMemManage
                  | SaveMemManage MemInfoData
                  | EndMemManage
                  | CommonNoOp

{-| Transform the state to a new state according to an action -}
update : CommonAction -> CommonState -> CommonState
update action s =
    case action of
        SetLog l            -> {s | log <- l}
        AppendToLog str     -> {s | log <- str::s.log}
        SetDeviceStatus c   -> {s | deviceStatus <- c}
        SetImportInfo i     -> {s | importInfo <- i}
        StartImportMedia id -> {s | importInfo <- ImportRequested id}
        SetMemInfo i        -> {s | memoryInfo <- i}
        StartMemManage      -> {s | memoryInfo <- MemInfoRequest}
        SaveMemManage d     -> {s | memoryInfo <- MemInfoSave d}
        EndMemManage        -> {s | memoryInfo <- NoMemInfo}
        -- GetState just twiddles the forceUpdate bit to make the state seem
        -- changed. This is so we can dropRepeats on the state signal but force
        -- an update through if we need to (like when the GUI is newly opened
        -- and definetely needs the state).
        GetState            -> {s | forceUpdate <- not s.forceUpdate}
        CommonNoOp          -> s

apply : List CommonAction -> CommonState -> CommonState
apply actions state = foldr update state actions
