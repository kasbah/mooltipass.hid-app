module CommonState where

-- Elm standard library
import List (..)
import String

-- local source
import Byte (..)
import KeyboardLayout(..)

{-| The background state excluding gui components -}
type alias CommonState =
    { deviceStatus : DeviceStatus
    , log          : List String
    , importInfo   : ImportInfo
    , memoryInfo   : MemInfo
    , getStringCmd : Maybe Int
    , setParameter : Maybe (Parameter, Byte)
    , getParameter : Maybe Parameter
    , settingsInfo : SettingsInfo
    , forceUpdate  : Bool
    }

default : CommonState
default =
    { deviceStatus = NotConnected
    , log          = []
    , importInfo   = NoImport
    , memoryInfo   = NoMemInfo
    , getStringCmd = Nothing
    , setParameter = Nothing
    , getParameter = Nothing
    , settingsInfo = emptySettingsInfo
    , forceUpdate  = True
    }

type alias Favorite = Maybe (FlashAddress, FlashAddress)

maxFavs = 14

emptyFavorites : List Favorite
emptyFavorites = repeat maxFavs Nothing

{-| Set-able parameters. Only some of these will be exposed to the user -}
type Parameter = UserInitKey
               | KeyboardLayout
               | UserInterTimeout
               | LockTimeoutEnable
               | LockTimeout
               | TouchDi
               | TouchWheelOs
               | TouchProxOs
               | OfflineMode
               | ScreenSaver
               | FlashScreen

encodeParameter : Parameter -> Int
encodeParameter p = case p of
    UserInitKey        -> 0x00
    KeyboardLayout     -> 0x01
    UserInterTimeout   -> 0x02
    LockTimeoutEnable  -> 0x03
    LockTimeout        -> 0x04
    TouchDi            -> 0x05
    TouchWheelOs       -> 0x06
    TouchProxOs        -> 0x07
    OfflineMode        -> 0x08
    ScreenSaver        -> 0x09
    FlashScreen        -> 0x0e
    _                  -> 0xFF

decodeParameter : Int -> Parameter
decodeParameter i = case i of
    0x00 -> UserInitKey       
    0x01 -> KeyboardLayout
    0x02 -> UserInterTimeout
    0x03 -> LockTimeoutEnable
    0x04 -> LockTimeout
    0x05 -> TouchDi
    0x06 -> TouchWheelOs
    0x07 -> TouchProxOs
    0x08 -> OfflineMode
    0x09 -> ScreenSaver
    0x0e -> FlashScreen
    _    -> KeyboardLayout


type MemInfo =
      MemInfo MemInfoData
    | MemInfoRequest
    | MemInfoSave MemInfoData
    | MemInfoWaitingForUser
    | MemInfoWaitingForDevice
    | MemInfoUnknownCardInserted
    | MemInfoUnknownCardWaitingForCpz
    | MemInfoUnknownCardCpz   ByteArray
    | MemInfoUnknownCardAdd   Card
    | MemInfoUnknownCardError ByteArray
    | NoMemInfo

type alias SettingsInfo =
    { keyboard    : Maybe Int
    , timeout     : Maybe Int
    , lockTimeoutEnable : Maybe Bool
    , lockTimeout : Maybe Int
    , offline     : Maybe Bool
    , screensaver : Maybe Bool
    , flashscreen : Maybe Bool
    }

emptySettingsInfo : SettingsInfo
emptySettingsInfo = SettingsInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing

updateSettingsInfo : Parameter -> Byte -> SettingsInfo -> SettingsInfo
updateSettingsInfo p b s = let bbool = not (b==0) in case p of
  KeyboardLayout       -> { s | keyboard    <- Just b }
  UserInterTimeout     -> { s | timeout     <- Just b }
  LockTimeout          -> { s | lockTimeout <- Just b }
  LockTimeoutEnable    -> { s | lockTimeoutEnable <- Just bbool }
  OfflineMode          -> { s | offline     <- Just bbool }
  ScreenSaver          -> { s | screensaver <- Just bbool }
  FlashScreen          -> { s | flashscreen <- Just bbool }
  _                    -> s

type alias ServiceName =
    { address    : FlashAddress
    , flags      : (Byte,Byte)
    , service    : ByteString
    }

type alias Login =
    { address      : FlashAddress
    , flags        : (Byte,Byte)
    , ctr          : (Byte, Byte, Byte)
    , description  : ByteString
    , login        : ByteString
    , password     : ByteArray
    , dateCreated  : (Byte, Byte)
    , dateLastUsed : (Byte, Byte)
    }

type alias Service = (ServiceName, List Login)

type alias MemInfoData =
    { credentials : List Service
    , favorites   : List Favorite
    , addresses   : List FlashAddress
    , ctr         : (Byte,Byte,Byte)
    , cards       : List Card
    , curCardCpz  : ByteArray
    }

type alias Card = { cpz : ByteArray
                  , ctrNonce : ByteArray
                  }

type DeviceStatus = NotConnected | Unlocked | NoCard | Locked | ManageMode | UnknownCard

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
    ManageMode   -> "device status: memory management mode"
    UnknownCard  -> "device status: unknown card present"

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
                  | GetStringCmd (Maybe Int)
                  | SetParameter (Maybe (Parameter, Byte))
                  | GetParameter (Maybe Parameter)
                  | CommonSettings SettingsInfo
                  | CommonNoOp

{-| Transform the state to a new state according to an action -}
update : CommonAction -> CommonState -> CommonState
update action s =
    case action of
        SetLog l            -> {s | log <- l}
        AppendToLog str     -> if length s.log < 5000
                               then {s | log <- str::s.log}
                               else {s | log <- take 5000 (str::s.log)}
        SetDeviceStatus c   -> {s | deviceStatus <- c}
        SetImportInfo i     -> {s | importInfo <- i}
        StartImportMedia id -> {s | importInfo <- ImportRequested id}
        SetMemInfo i        -> {s | memoryInfo <- i}
        StartMemManage      -> {s | memoryInfo <- MemInfoRequest}
        SaveMemManage d     -> {s | memoryInfo <- MemInfoSave d}
        EndMemManage        -> {s | memoryInfo <- NoMemInfo}
        GetStringCmd mc     -> case mc of
                                   Nothing -> {s | getStringCmd <- Nothing }
                                   Just c  -> {s | getStringCmd <- Just c}
        SetParameter mpb    -> case mpb of
                                   Nothing -> { s | setParameter <- Nothing }
                                   Just pb -> {s | setParameter <- Just pb}
        GetParameter mp     -> case mp of
                                   Nothing -> {s | getParameter <- Nothing }
                                   Just p  -> {s | getParameter <- Just p}
        CommonSettings settings   -> {s | settingsInfo <- settings}
        -- GetState just twiddles the forceUpdate bit to make the state seem
        -- changed. This is so we can dropRepeats on the state signal but force
        -- an update through if we need to (like when the GUI is newly opened
        -- and definetely needs the state).
        GetState            -> {s | forceUpdate <- not s.forceUpdate}
        CommonNoOp          -> s

apply : List CommonAction -> CommonState -> CommonState
apply actions state = foldr update state actions
