module ToGuiMessage where

-- local source
import CommonState (..)

type alias ToGuiMessage = { setLog          : (List String)
                          , setDeviceStatus : Int
                          , setImportInfo   : (Int,FileId,Int,Int)
                          , setMemInfo      : (Int, Maybe MemInfoData)
                          , setKbInfo       : Maybe Int
                          , getKbInfo       : Maybe ()
                          }

encode : CommonState -> ToGuiMessage
encode s =
    { setLog = s.log
    , setDeviceStatus = case s.deviceStatus of
                    NotConnected -> 0
                    Unlocked     -> 1
                    NoCard       -> 2
                    Locked       -> 3
                    ManageMode   -> 4
                    UnknownCard  -> 5
    , setImportInfo = case s.importInfo of
        NoImport           -> (0,"",0,0)
        ImportRequested id -> (1,id,0,0)
        Importing id i1 i2 -> (2,id,i1,i2)
        Imported id        -> (3,id,0,0)
        ImportError s      -> (4,s ,0,0)
    , setMemInfo = case s.memoryInfo of
        NoMemInfo               -> (0, Nothing)
        MemInfoRequest          -> (1, Nothing)
        MemInfoWaitingForUser   -> (2, Nothing)
        MemInfoWaitingForDevice -> (3, Nothing)
        MemInfo d               -> (4, Just d)
    , setKbInfo = case s.setKeyboard of
        0                       -> Nothing
        x                       -> Just x
    , getKbInfo = s.getKeyboard
    }

decode : ToGuiMessage -> List CommonAction
decode msg=
    let setDeviceStatus =
        case msg.setDeviceStatus of
            0 -> NotConnected
            1 -> Unlocked
            2 -> NoCard
            3 -> Locked
            4 -> ManageMode
            5 -> UnknownCard
        setImportInfo = case msg.setImportInfo of
           (0,"",0,0)   -> NoImport
           (1,id,0,0)   -> ImportRequested id
           (2,id,i1,i2) -> Importing id i1 i2
           (3,id,0,0)   -> Imported id
           (4,s,0,0)    -> ImportError s
        setMemInfo = case msg.setMemInfo of
            (0, Nothing) -> NoMemInfo
            (1, Nothing) -> MemInfoRequest
            (2, Nothing) -> MemInfoWaitingForUser
            (3, Nothing) -> MemInfoWaitingForDevice
            (4, Just d)  -> MemInfo d
        kb = case msg.setKbInfo of
            Nothing -> SetKeyboard 0
            Just x -> SetKeyboard x
    in  [ SetLog msg.setLog
        , SetDeviceStatus setDeviceStatus
        , SetImportInfo setImportInfo
        , SetMemInfo setMemInfo
        , kb
        , GetKeyboard msg.getKbInfo
        ]
