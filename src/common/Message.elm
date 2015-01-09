module Message where

-- Elm standard library
import Maybe
import Dict
import List ((::))
import List

-- local source
import State (..)

type alias Message = { setLog : Maybe (List String)
                     , setConnected : Maybe Int
                     }

empty : Message
empty = { setLog = Nothing
               , setConnected = Nothing
               }

encode : Action -> Message
encode action =
    case action of
        SetLog log     -> {empty | setLog <- Just log}
        SetConnected c ->
            {empty | setConnected <-
                case c of
                    NotConnected -> Just 0
                    Connected    -> Just 1
                    NoCard       -> Just 2
                    NoPin        -> Just 3
                    _            -> Nothing
            }
        (ChangeTab t)    -> empty
        ClickIcon        -> empty
        NoOp             -> empty

decode : Message -> Action
decode message =
    let decode' {setLog, setConnected} =
            Maybe.oneOf [ Maybe.map SetLog setLog
                        , Maybe.map connectedFromInt setConnected
                        ]
        connectedFromInt s =
            case s of
                    0 -> SetConnected NotConnected
                    1 -> SetConnected Connected
                    2 -> SetConnected NoCard
                    3 -> SetConnected NoPin
                    _ -> NoOp
    in Maybe.withDefault NoOp (decode' message)

