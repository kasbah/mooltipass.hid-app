module Communication where

-- Elm standard library
import Maybe
import Dict
import List ((::))
import List

-- local source
import State (..)

type alias Message = { appendToLog : Maybe String
                     , setConnected : Maybe Int
                     }

emptyMessage : Message
emptyMessage = { appendToLog = Nothing
               , setConnected = Nothing
               }

port toGUI : Signal Message

encode : Action -> Message
encode action =
    case action of
        (ChangeTab t)    -> emptyMessage
        (SetConnected c) -> emptyMessage-- | setConnected <- intFromCon c}
        ClearLog         -> emptyMessage
        ClickIcon        -> emptyMessage
        AppendToLog str  -> {emptyMessage | appendToLog <- Just str}
        NoOp             -> emptyMessage

decode : Message -> Action
decode message =
    let decode' {appendToLog, setConnected} =
            Maybe.oneOf [ Maybe.map AppendToLog appendToLog
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

