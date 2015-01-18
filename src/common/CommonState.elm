module CommonState where

-- Elm standard library
import List ((::))
import List

{-| The background state excluding gui components -}
type alias CommonState =
    { connected : ConnectState
    , log       : List String
    }

default : CommonState
default =
    { connected = NotConnected
    , log       = []
    }

type ConnectState = NotConnected | Connected | NoCard | NoPin

{-| All actions that can be performed to change state -}
type CommonAction = SetLog (List String)
                  | SetConnected ConnectState
                  | AppendToLog String
                  | GetState
                  | CommonNoOp

{-| Transform the state to a new state according to an action -}
update : CommonAction -> CommonState -> CommonState
update action s =
    case action of
        (SetLog l)        -> {s | log <- l}
        (AppendToLog str) -> {s | log <- str::s.log}
        (SetConnected c)  -> {s | connected <- c}
        GetState          -> s
        CommonNoOp        -> s

apply : List CommonAction -> CommonState -> CommonState
apply actions state = List.foldr update state actions
