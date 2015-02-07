module CommonState where

-- Elm standard library
import List ((::))
import List

{-| The background state excluding gui components -}
type alias CommonState =
    { connected : ConnectState
    , log       : List String
    , transferInfo  : TransferInfo
    }

default : CommonState
default =
    { connected = NotConnected
    , log       = []
    , transferInfo = NoTransfer
    }

type ConnectState = NotConnected | Connected | NoCard | NoPin

type TransferInfo =
      ImportRequested FileId
    | Importing FileId Int Int
    | Imported FileId
    | TransferError String
    | NoTransfer

type alias FileId = String

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
                  | SetTransferInfo TransferInfo
                  | StartImportMedia FileId
                  | CommonNoOp

{-| Transform the state to a new state according to an action -}
update : CommonAction -> CommonState -> CommonState
update action s =
    case action of
        (SetLog l)         -> {s | log <- l}
        (AppendToLog str)  -> {s | log <- str::s.log}
        (SetConnected c)   -> {s | connected <- c}
        GetState           -> s
        SetTransferInfo i -> {s | transferInfo <- i}
        StartImportMedia id ->
            case s.transferInfo of
                NoTransfer      -> {s | transferInfo <- ImportRequested id}
                Imported _      -> {s | transferInfo <- ImportRequested id}
                TransferError _ -> {s | transferInfo <- ImportRequested id}
                _ -> s
        CommonNoOp         -> s

apply : List CommonAction -> CommonState -> CommonState
apply actions state = List.foldr update state actions
