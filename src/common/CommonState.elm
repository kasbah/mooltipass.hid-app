module CommonState where

-- Elm standard library
import List ((::))
import List

{-| The background state excluding gui components -}
type alias CommonState =
    { connected : ConnectState
    , log       : List String
    , transferMedia  : Transfer
    }

default : CommonState
default =
    { connected = NotConnected
    , log       = []
    , transferMedia  = NoTransfer
    }

type ConnectState = NotConnected | Connected | NoCard | NoPin

type Transfer =
      ImportRequested FilePath
    | Importing FilePath Int
    | Imported FilePath
    | TransferError String
    | NoTransfer

type alias FilePath = String

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
                  | SetTransferMedia Transfer
                  | StartImportMedia String
                  | CommonNoOp

{-| Transform the state to a new state according to an action -}
update : CommonAction -> CommonState -> CommonState
update action s =
    case action of
        (SetLog l)         -> {s | log <- l}
        (AppendToLog str)  -> {s | log <- str::s.log}
        (SetConnected c)   -> {s | connected <- c}
        GetState           -> s
        SetTransferMedia t -> {s | transferMedia <- t}
        StartImportMedia p ->
            case s.transferMedia of
                NoTransfer      -> {s | transferMedia <- ImportRequested p}
                Imported _      -> {s | transferMedia <- ImportRequested p}
                TransferError _ -> {s | transferMedia <- ImportRequested p}
                _ -> s
        CommonNoOp         -> s

apply : List CommonAction -> CommonState -> CommonState
apply actions state = List.foldr update state actions
