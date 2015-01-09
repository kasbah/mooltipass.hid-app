module Message where

-- local source
import CommonState (..)

type alias Message = { setLog : (List String)
                     , setConnected : Int
                     }

encode : CommonState -> Message
encode bg =
    { setLog = bg.log
    , setConnected = case bg.connected of
                    NotConnected -> 0
                    Connected    -> 1
                    NoCard       -> 2
                    NoPin        -> 3
    }

decode : Message -> CommonState
decode msg = apply (decode' msg) default

decode' : Message -> List CommonAction
decode' msg=
    let setConnected =
        case msg.setConnected of
            0 -> NotConnected
            1 -> Connected
            2 -> NoCard
            3 -> NoPin
    in  [SetLog msg.setLog, SetConnected setConnected]
