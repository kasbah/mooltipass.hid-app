module CommonState where

-- Elm standard library
import List (..)
import String

-- local source

{-| The background state excluding gui components -}
type alias CommonState =
    { connected    : ConnectState
    , log          : List String
    , importInfo : ImportInfo
    , memoryInfo   : MemoryInfo
    , forceUpdate  : Bool
    }

default : CommonState
default =
    { connected    = NotConnected
    , log          = []
    , importInfo = NoImport
    , memoryInfo   = exampleMemoryInfo
    , forceUpdate  = True
    }

type alias Favorite = Maybe (String, String)

emptyFavorites = [Nothing,Nothing,Nothing,Nothing,Nothing
                 ,Nothing,Nothing,Nothing,Nothing,Nothing
                 ,Nothing,Nothing,Nothing,Nothing,Nothing]

type alias MemoryInfo =
    { credentials : List (String, List String)
    , favorites   : List Favorite
    }

emptyMemoryInfo =
    { credentials = []
    , favorites   = emptyFavorites
    }

exampleMemoryInfo =
    { credentials = [ ("github.com", ["kasbah", "monostable"])
                    , ("oshpark.com",["kaspar.emanuel@gmail.com"])
                    , ("amazon.com" ,["kaspar.bumke+nu-server@gmail.com"])
                    , ("oshpark.com",["kaspar.emanuel@gmail.com"])
                    , ("seafile.cc" ,["kaspar.bumke@gmail.com"])
                    , ("example.com" ,[ "me@example.com"])
                    , ("eggsample.com" , ["eggs@sample.com"])
                    ]
    , favorites   = [ Just ("github.com", "kasbah")
                    , Just ("oshpark.com", "kaspar.emanuel@gmail.com")
                    , Just ("amazon.com" , "kaspar.bumke+nu-server@gmail.com")
                    , Just ("seafile.cc" , "kaspar.bumke@gmail.com")
                    , Just ("example.com" , "me@example.com")
                    , Just ("eggsample.com" , "eggs@sample.com")
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    ]
    }

type ConnectState = NotConnected | Connected | NoCard | NoPin

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

connectToLog : ConnectState -> String
connectToLog c = case c of
    NotConnected -> "device disconnected"
    Connected    -> "device status: unlocked"
    NoCard       -> "device status: no card present"
    NoPin        -> "device status: locked"

{-| All actions that can be performed to change state -}
type CommonAction = SetLog (List String)
                  | SetConnected ConnectState
                  | AppendToLog String
                  | GetState
                  | SetImportInfo ImportInfo
                  | StartImportMedia FileId
                  | SetMemoryInfo MemoryInfo
                  | CommonNoOp

{-| Transform the state to a new state according to an action -}
update : CommonAction -> CommonState -> CommonState
update action s =
    case action of
        SetLog l            -> {s | log <- l}
        AppendToLog str     -> {s | log <- str::s.log}
        SetConnected c      -> {s | connected <- c}
        SetImportInfo i   -> {s | importInfo <- i}
        StartImportMedia id -> {s | importInfo <- ImportRequested id}
        SetMemoryInfo i     -> {s | memoryInfo <- i}
        -- GetState just twiddles the forceUpdate bit to make the state seem
        -- changed. This is so we can dropRepeats on the state signal but force
        -- an update through if we need to (like when the GUI is closed and
        -- then re-opened).
        GetState            -> {s | forceUpdate <- not s.forceUpdate}
        CommonNoOp          -> s

apply : List CommonAction -> CommonState -> CommonState
apply actions state = foldr update state actions
