module Message where

-- Elm standard library
import Maybe
import Dict
import List ((::))
import List

-- local source
import State (..)

type alias Message = { setLog : (List String)
                     , setConnected : Int
                     }

encode : BgState -> Message
encode bg =
    { setLog = bg.log
    , setConnected = case bg.connect of
                    NotConnected -> 0
                    Connected    -> 1
                    NoCard       -> 2
                    NoPin        -> 3
    }

decode : Message -> BgState
decode msg =
    { log = msg.setLog
    , connect = case msg.setConnected of
                    0 -> NotConnected
                    1 -> Connected
                    2 -> NoCard
                    3 -> NoPin
    }
