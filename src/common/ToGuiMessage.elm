module ToGuiMessage where

-- local source
import CommonState (..)

type alias ToGuiMessage = { setLog : (List String)
                          , setConnected : Int
                          , setTransferMedia  : (Int,String,Int)
                          }

encode : CommonState -> ToGuiMessage
encode s =
    { setLog = s.log
    , setConnected = case s.connected of
                    NotConnected -> 0
                    Connected    -> 1
                    NoCard       -> 2
                    NoPin        -> 3
    , setTransferMedia = case s.transferMedia of
        NoTransfer         -> (0,"",0)
        ImportRequested p -> (1,p,0)
        Importing p i      -> (2,p,i)
        Imported p         -> (3,p,0)
        TransferError s    -> (4,s,0)
    }

decode : ToGuiMessage -> List CommonAction
decode msg=
    let setConnected =
        case msg.setConnected of
            0 -> NotConnected
            1 -> Connected
            2 -> NoCard
            3 -> NoPin
        setTransfer = case msg.setTransferMedia of
           (0,"",0) -> NoTransfer
           (1,p,0)  -> ImportRequested p
           (2,p,i)  -> Importing p i
           (3,p,0)  -> Imported p
           (4,s,0)  -> TransferError s
    in  [ SetLog msg.setLog
        , SetConnected setConnected
        , SetTransferMedia setTransfer
        ]
