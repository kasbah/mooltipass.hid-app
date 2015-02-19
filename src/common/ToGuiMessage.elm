module ToGuiMessage where

-- local source
import CommonState (..)

type alias ToGuiMessage = { setLog          : (List String)
                          , setDeviceStatus : Int
                          , setImportInfo   : (Int,FileId,Int,Int)
                          , setMemoryInfo   : (Int, Maybe MemoryInfoData)
                          }

encode : CommonState -> ToGuiMessage
encode s =
    { setLog = s.log
    , setDeviceStatus = case s.deviceStatus of
                    NotConnected -> 0
                    Unlocked     -> 1
                    NoCard       -> 2
                    Locked       -> 3
    , setImportInfo = case s.importInfo of
        NoImport           -> (0,"",0,0)
        ImportRequested id -> (1,id,0,0)
        Importing id i1 i2 -> (2,id,i1,i2)
        Imported id        -> (3,id,0,0)
        ImportError s      -> (4,s ,0,0)
    , setMemoryInfo = case s.memoryInfo of
        NoMemoryInfo       -> (0, Nothing)
        MemInfoRequest     -> (1, Nothing)
        MemInfoWaiting     -> (2, Nothing)
        MemoryInfo d       -> (3, Just d)

    }

decode : ToGuiMessage -> List CommonAction
decode msg=
    let setDeviceStatus =
        case msg.setDeviceStatus of
            0 -> NotConnected
            1 -> Unlocked
            2 -> NoCard
            3 -> Locked
        setImportInfo = case msg.setImportInfo of
           (0,"",0,0)   -> NoImport
           (1,id,0,0)   -> ImportRequested id
           (2,id,i1,i2) -> Importing id i1 i2
           (3,id,0,0)   -> Imported id
           (4,s,0,0)    -> ImportError s
        setMemoryInfo = case msg.setMemoryInfo of
            (0,_)      -> NoMemoryInfo
            (1,_)      -> MemInfoRequest
            (2,_)      -> MemInfoWaiting
            (3,Just d) -> MemoryInfo d
    in  [ SetLog msg.setLog
        , SetDeviceStatus setDeviceStatus
        , SetImportInfo setImportInfo
        , SetMemoryInfo setMemoryInfo
        ]
