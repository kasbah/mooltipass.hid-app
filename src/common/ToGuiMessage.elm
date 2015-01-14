module ToGuiMessage where

-- local source
import CommonState (..)

type alias ToGuiMessage = { setLog : (List String)
                          , setConnected : Int
                          }

encode : CommonState -> ToGuiMessage
encode com =
    { setLog = com.log
    , setConnected = case com.connected of
                    NotConnected -> 0
                    Connected    -> 1
                    NoCard       -> 2
                    NoPin        -> 3
    }

decode : ToGuiMessage -> List CommonAction
decode msg=
    let setConnected =
        case msg.setConnected of
            0 -> NotConnected
            1 -> Connected
            2 -> NoCard
            3 -> NoPin
    in  [SetLog msg.setLog, SetConnected setConnected]
