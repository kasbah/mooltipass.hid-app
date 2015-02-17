module ToGuiMessage where

-- local source
import CommonState (..)

type alias ToGuiMessage = { setLog          : (List String)
                          , setConnected    : Int
                          , setImportInfo : (Int,FileId,Int,Int)
                          , setMemoryInfo   : MemoryInfo
                          }

encode : CommonState -> ToGuiMessage
encode s =
    { setLog = s.log
    , setConnected = case s.connected of
                    NotConnected -> 0
                    Connected    -> 1
                    NoCard       -> 2
                    NoPin        -> 3
    , setImportInfo = case s.importInfo of
        NoImport         -> (0,"",0,0)
        ImportRequested id -> (1,id,0,0)
        Importing id i1 i2 -> (2,id,i1,i2)
        Imported id        -> (3,id,0,0)
        ImportError s    -> (4,s ,0,0)
    , setMemoryInfo = s.memoryInfo
    }

decode : ToGuiMessage -> List CommonAction
decode msg=
    let setConnected =
        case msg.setConnected of
            0 -> NotConnected
            1 -> Connected
            2 -> NoCard
            3 -> NoPin
        setImportInfo = case msg.setImportInfo of
           (0,"",0,0)   -> NoImport
           (1,id,0,0)   -> ImportRequested id
           (2,id,i1,i2) -> Importing id i1 i2
           (3,id,0,0)   -> Imported id
           (4,s,0,0)    -> ImportError s
    in  [ SetLog msg.setLog
        , SetConnected setConnected
        , SetImportInfo setImportInfo
        , SetMemoryInfo msg.setMemoryInfo
        ]
