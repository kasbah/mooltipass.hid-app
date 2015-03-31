module DevicePacket where

-- Elm standard library
import Result (..)
import Result
import List
import List ((::))
import Maybe
import String
import Char
import Bitwise (and)

-- local source
import Byte (..)

cmd_DEBUG               = 0x01
cmd_PING                = 0x02
cmd_VERSION             = 0x03
cmd_CONTEXT             = 0x04
cmd_GET_LOGIN           = 0x05
cmd_GET_PASSWORD        = 0x06
cmd_SET_LOGIN           = 0x07
cmd_SET_PASSWORD        = 0x08
cmd_CHECK_PASSWORD      = 0x09
cmd_ADD_CONTEXT         = 0x0A
cmd_EXPORT_FLASH        = 0x30
cmd_EXPORT_FLASH_END    = 0x31
cmd_IMPORT_FLASH_BEGIN  = 0x32
cmd_IMPORT_FLASH        = 0x33
cmd_IMPORT_FLASH_END    = 0x34
cmd_EXPORT_EEPROM       = 0x35
cmd_EXPORT_EEPROM_END   = 0x36
cmd_IMPORT_EEPROM_BEGIN = 0x37
cmd_IMPORT_EEPROM       = 0x38
cmd_IMPORT_EEPROM_END   = 0x39
cmd_ERASE_EEPROM        = 0x40
cmd_ERASE_FLASH         = 0x41
cmd_ERASE_SMC           = 0x42
cmd_DRAW_BITMAP         = 0x43
cmd_SET_FONT            = 0x44
cmd_EXPORT_FLASH_START  = 0x45
cmd_EXPORT_EEPROM_START = 0x46
cmd_SET_BOOTLOADER_PWD  = 0x47
cmd_JUMP_TO_BOOTLOADER  = 0x48
cmd_CLONE_SMARTCARD     = 0x49
cmd_STACK_FREE          = 0x4A
cmd_GET_RANDOM_NUMBER   = 0x4B
cmd_START_MEMORYMGMT    = 0x50
cmd_END_MEMORYMGMT      = 0x51
cmd_IMPORT_MEDIA_START  = 0x52
cmd_IMPORT_MEDIA        = 0x53
cmd_IMPORT_MEDIA_END    = 0x54
cmd_READ_FLASH_NODE     = 0x55
cmd_WRITE_FLASH_NODE    = 0x56
cmd_SET_FAVORITE        = 0x57
cmd_SET_STARTING_PARENT = 0x58
cmd_SET_CTRVALUE        = 0x59
cmd_ADD_CARD_CPZ_CTR    = 0x5A
cmd_GET_CARD_CPZ_CTR    = 0x5B
cmd_CARD_CPZ_CTR_PACKET = 0x5C
cmd_SET_MOOLTIPASS_PARM = 0x5D
cmd_GET_MOOLTIPASS_PARM = 0x5E
cmd_GET_FAVORITE        = 0x5F
cmd_RESET_CARD          = 0x60
cmd_READ_CARD_LOGIN     = 0x61
cmd_READ_CARD_PASS      = 0x62
cmd_SET_CARD_LOGIN      = 0x63
cmd_SET_CARD_PASS       = 0x64
cmd_GET_STARTING_PARENT = 0x66
cmd_GET_CTRVALUE        = 0x67
cmd_ADD_UNKNOWN_CARD    = 0x68
cmd_USB_KEYBOARD_PRESS  = 0x69
cmd_MOOLTIPASS_STATUS   = 0x70

--cmd_FUNCTIONAL_TEST_RES = 0x71
--cmd_SET_DATE            = 0x72
--cmd_SET_UID             = 0x74
--cmd_GET_UID             = 0x75
--cmd_SET_DATA_SERVICE    = 0x78
--cmd_ADD_DATA_SERVICE    = 0x79
--cmd_WRITE_32B_IN_DN     = 0x7A
--cmd_READ_32B_IN_DN      = 0x7B
--cmd_GET_30_FREE_SLOTS   = 0xAB
--cmd_GET_DN_START_PARENT = 0xAC
--cmd_SET_DN_START_PARENT = 0xAD
--cmd_END_MEMORYMGMT      = 0xAE

{-| The type of packets we send from the app to the device -}
type OutgoingPacket =
     OutgoingDebug              ByteString
   | OutgoingPing
   | OutgoingGetVersion
   | OutgoingSetContext         ByteString
   | OutgoingGetLogin
   | OutgoingGetPassword
   | OutgoingSetLogin           ByteString
   | OutgoingSetPassword        ByteString
   | OutgoingCheckPassword
   | OutgoingAddContext         ByteString
   | OutgoingExportFlashStart
   | OutgoingExportFlash
   | OutgoingExportFlashEnd
   | OutgoingImportFlashStart   FlashSpace
   | OutgoingImportFlash        ByteArray
   | OutgoingImportFlashEnd
   | OutgoingExportEepromStart
   | OutgoingExportEeprom
   | OutgoingExportEepromEnd
   | OutgoingImportEepromStart
   | OutgoingImportEeprom       ByteString
   | OutgoingImportEepromEnd
   | OutgoingGetRandomNumber
   | OutgoingMemManageModeStart
   | OutgoingMemManageModeEnd
   | OutgoingImportMediaStart
   | OutgoingImportMedia        ByteArray
   | OutgoingImportMediaEnd
   | OutgoingReadFlashNode      FlashAddress
   | OutgoingWriteFlashNode     FlashAddress Byte ByteArray
   | OutgoingSetFavorite        (Byte,(FlashAddress,FlashAddress))
   | OutgoingSetStartingParent  FlashAddress
   -- CPZ = code protected zone
   -- CTR = counter value for Eeprom
   | OutgoingSetCtrValue        (Byte, Byte, Byte)
   | OutgoingAddCpzCtr          CpzCtrLutEntry
   | OutgoingGetCpzCtrValues
   | OutgoingSetParameter       Parameter Byte
   | OutgoingGetParameter       Parameter
   | OutgoingGetFavorite        Byte
   | OutgoingResetCard          (Byte, Byte)
   | OutgoingGetCardLogin
   | OutgoingGetCardPassword
   | OutgoingSetCardLogin       ByteString
   | OutgoingSetCardPassword    ByteString
   | OutgoingGetStartingParent
   | OutgoingGetCtrValue
   | OutgoingAddNewCard
   | OutgoingGetStatus
-- disabled developer types:
    --OutgoingEraseEeprom      -> cmd_ERASE_EEPROM
    --OutgoingEraseFlash       -> cmd_ERASE_FLASH
    --OutgoingEraseSmc         -> cmd_ERASE_SMC
    --OutgoingDrawBitmap       -> cmd_DRAW_BITMAP
    --OutgoingSetFont          -> cmd_SET_FONT
    --OutgoingSetBootloaderPwd -> cmd_SET_BOOTLOADER_PWD
    --OutgoingJumpToBootloader -> cmd_JUMP_TO_BOOTLOADER
    --OutgoingCloneSmartcard   -> cmd_CLONE_SMARTCARD
    --OutgoingStackFree        -> cmd_STACK_FREE
    --OutgoingUsbKeyboardPress -> cmd_USB_KEYBOARD_PRESS


{-| The type of packets we receive from the device -}
type ReceivedPacket =
      ReceivedDebug             ByteString
    | ReceivedPing              ByteString
    | ReceivedGetVersion        MpVersion
    | ReceivedSetContext        SetContextReturn
    | ReceivedGetLogin          (Maybe ByteString)
    | ReceivedGetPassword       (Maybe ByteString)
    | ReceivedSetLogin          ReturnCode
    | ReceivedSetPassword       ReturnCode
    | ReceivedCheckPassword     CheckPasswordReturn
    | ReceivedAddContext        ReturnCode
    | ReceivedExportFlashStart  ReturnCode
    | ReceivedExportFlash       ByteString
    | ReceivedExportFlashEnd
    | ReceivedImportFlashStart  ReturnCode
    | ReceivedImportFlash       ReturnCode
    | ReceivedImportFlashEnd    ReturnCode
    | ReceivedExportEepromStart ReturnCode
    | ReceivedExportEeprom      ByteString
    | ReceivedExportEepromEnd
    | ReceivedImportEepromStart ReturnCode
    | ReceivedImportEeprom      ReturnCode
    | ReceivedImportEepromEnd   ReturnCode
    | ReceivedGetRandomNumber   ByteString
    | ReceivedManageModeStart   ReturnCode
    | ReceivedManageModeEnd     ReturnCode
    | ReceivedImportMediaStart  ReturnCode
    | ReceivedImportMediaEnd    ReturnCode
    | ReceivedImportMedia       ReturnCode
    | ReceivedReadFlashNode     ByteArray
    | ReceivedWriteFlashNode    ReturnCode
    | ReceivedSetFavorite       ReturnCode
    | ReceivedSetStartingParent ReturnCode
    | ReceivedSetCtrValue       ReturnCode
    | ReceivedAddCpzCtr         ReturnCode
    | ReceivedGetCpzCtrValues   (Maybe ByteString)
    | ReceivedCpzCtrPacketExport CpzCtrLutEntry
    | ReceivedSetParameter      ReturnCode
    | ReceivedGetParameter      (Maybe ByteString)
    | ReceivedGetFavorite       (FlashAddress, FlashAddress)
    | ReceivedResetCard         ReturnCode
    | ReceivedGetCardLogin      (Maybe ByteString)
    | ReceivedGetCardPassword   (Maybe ByteString)
    | ReceivedSetCardLogin      ReturnCode
    | ReceivedSetCardPassword   ReturnCode
    | ReceivedGetStartingParent FlashAddress
    | ReceivedGetCtrValue       (Maybe ByteString)
    | ReceivedAddNewCard        ReturnCode

{-| Carries firmware version and flash memory size -}
type alias MpVersion = { flashMemSize : Byte
                       , version : ByteString
                       }

{-| Code protected zone look-up-table entry -}
type alias CpzCtrLutEntry = { cpz : ByteString
                            , ctrNonce : ByteString
                            }

{-| Return for 'ReceivedCheckPassword' -}
type CheckPasswordReturn = Incorrect | Correct | RequestBlocked

{-| Return for 'ReceivedSetContext' -}
type SetContextReturn = UnknownContext | ContextSet | NoCardForContext

{-| You dun it? -}
type ReturnCode = Done | NotDone

{-| Set-able parameters. Only some of these will be exposed to the user -}
type Parameter = UserInitKey
               | KeyboardLayout
               | UserInterTimeout
               | LockTimeoutEnable
               | LockTimeout
               | TouchDi
               | TouchWheelOs
               | TouchProxOs
               | OfflineMode

{-| The flash space is divided into these two regions -}
type FlashSpace = FlashUserSpace | FlashGraphicsSpace

{-| Convert a packet generated in our application to a list of Ints to send out
    a port for chrome.hid.send -}
toInts : OutgoingPacket -> List Int
toInts msg =
    -- the packet format is [payload-size, message-type, payload ... ]
    let byteString msgType s = String.length s::msgType::stringToInts s
        byteArray msgType a  = List.length a::msgType::a
        byteStringNull msgType s =
            (String.length s + 1)::msgType::stringToInts s ++ [0]
        zeroSize msgType     = [0, msgType]
        param p              = case p of
            UserInitKey        -> 0x00
            KeyboardLayout     -> 0x01
            UserInterTimeout   -> 0x02
            LockTimeoutEnable  -> 0x03
            LockTimeout        -> 0x04
            TouchDi            -> 0x05
            TouchWheelOs       -> 0x06
            TouchProxOs        -> 0x07
            OfflineMode        -> 0x08
    in case msg of
        OutgoingDebug       s  -> byteString cmd_DEBUG s
        OutgoingPing           -> zeroSize cmd_PING
        OutgoingGetVersion     -> zeroSize cmd_VERSION
        OutgoingSetContext  s  -> byteStringNull cmd_CONTEXT s
        OutgoingGetLogin       -> zeroSize cmd_GET_LOGIN
        OutgoingGetPassword    -> zeroSize cmd_GET_PASSWORD
        OutgoingSetLogin    s  -> byteStringNull cmd_SET_LOGIN s
        OutgoingSetPassword s  -> byteStringNull cmd_SET_PASSWORD s
        OutgoingCheckPassword  -> zeroSize cmd_CHECK_PASSWORD
        OutgoingAddContext  s  -> byteStringNull cmd_ADD_CONTEXT s
        OutgoingExportFlash    -> zeroSize cmd_EXPORT_FLASH
        OutgoingExportFlashEnd -> zeroSize cmd_EXPORT_FLASH_END
        OutgoingImportFlashStart space ->
            [ 1
            , cmd_IMPORT_FLASH_BEGIN
            , case space of
                FlashUserSpace     -> 0x00
                FlashGraphicsSpace -> 0x01
            ]
        OutgoingImportFlash  s        -> byteArray cmd_IMPORT_FLASH s
        OutgoingImportFlashEnd        -> zeroSize cmd_IMPORT_FLASH_END
        OutgoingExportEeprom          -> zeroSize cmd_EXPORT_EEPROM
        OutgoingExportEepromEnd       -> zeroSize cmd_EXPORT_EEPROM_END
        OutgoingImportEepromStart     -> zeroSize cmd_IMPORT_EEPROM_BEGIN
        OutgoingImportEeprom s        -> byteString cmd_IMPORT_EEPROM s
        OutgoingImportEepromEnd       -> zeroSize cmd_IMPORT_EEPROM_END
        OutgoingExportFlashStart      -> zeroSize cmd_EXPORT_FLASH_START
        OutgoingExportEepromStart     -> zeroSize cmd_EXPORT_EEPROM_START
        OutgoingGetRandomNumber       -> zeroSize cmd_GET_RANDOM_NUMBER
        OutgoingMemManageModeStart    -> zeroSize cmd_START_MEMORYMGMT
        OutgoingMemManageModeEnd      -> zeroSize cmd_END_MEMORYMGMT
        OutgoingImportMediaStart      -> zeroSize cmd_IMPORT_MEDIA_START
        OutgoingImportMedia  a        -> byteArray cmd_IMPORT_MEDIA a
        OutgoingImportMediaEnd        -> zeroSize cmd_IMPORT_MEDIA_END
        OutgoingReadFlashNode (a1,a2) -> [2, cmd_READ_FLASH_NODE, a1, a2]
        OutgoingWriteFlashNode (a1,a2) n ba ->
            (List.length ba + 3)::cmd_WRITE_FLASH_NODE::a1::a2::n::ba
        OutgoingSetFavorite (id,((p1,p2),(c1,c2))) ->
            [5, cmd_SET_FAVORITE, id, p1, p2, c1, c2]
        OutgoingSetStartingParent (a1,a2)    -> [2, cmd_SET_STARTING_PARENT, a1, a2]
        OutgoingSetCtrValue (ctr1,ctr2,ctr3) -> [3, cmd_SET_CTRVALUE, ctr1, ctr2, ctr3]
        OutgoingAddCpzCtr c -> 24::cmd_ADD_CARD_CPZ_CTR::stringToInts c.cpz ++ stringToInts c.ctrNonce
        OutgoingGetCpzCtrValues    -> zeroSize cmd_GET_CARD_CPZ_CTR
        OutgoingSetParameter p b   -> [2, cmd_SET_MOOLTIPASS_PARM, param p, b]
        OutgoingGetParameter p     -> [1, cmd_GET_MOOLTIPASS_PARM, param p]
        OutgoingGetFavorite  b     -> [1, cmd_GET_FAVORITE, b]
        OutgoingResetCard (b1,b2)  -> [2, cmd_RESET_CARD, b1, b2]
        OutgoingGetCardLogin       -> zeroSize cmd_READ_CARD_LOGIN
        OutgoingGetCardPassword    -> zeroSize cmd_READ_CARD_PASS
        OutgoingSetCardLogin s     -> byteStringNull cmd_SET_CARD_LOGIN s
        OutgoingSetCardPassword s  -> byteStringNull cmd_SET_CARD_PASS s
        OutgoingGetStartingParent  -> zeroSize cmd_GET_STARTING_PARENT
        OutgoingGetCtrValue        -> zeroSize cmd_GET_CTRVALUE
        OutgoingAddNewCard         -> zeroSize cmd_ADD_UNKNOWN_CARD
        OutgoingGetStatus          -> zeroSize cmd_MOOLTIPASS_STATUS

{-| Convert a list of ints received through a port from chrome.hid.receive into
    a packet we can interpret -}
fromInts : List Int -> Result Error ReceivedPacket
fromInts (size::m::payload) =
    let doneOrNotDone constructor name =
            if size /= 1
            then Err <| "Invalid data size for '" ++ name ++ "'"
            else case List.head payload of
                    0x00 -> Ok <| constructor NotDone
                    0x01 -> Ok <| constructor Done
                    _    -> Err <| "Invalid data for '" ++ name ++ "'"
        maybeByteString constructor name =
            if size <= 0
            then Err <| "Zero data returned for '" ++ name ++ "'"
            else if size == 1 && List.head payload == 0x00
                 then Ok <| constructor Maybe.Nothing
                 else Result.map (constructor << Maybe.Just)
                        (toByteString size payload)
        maybeByteStringNull constructor name =
            if size <= 0
            then Err <| "Zero data returned for '" ++ name ++ "'"
            else if size == 1 && List.head payload == 0x00
                 then Ok <| constructor Maybe.Nothing
                 else Result.map (constructor << Maybe.Just)
                        (toByteString (size - 1) payload)
    in
        if size > List.length payload
        then Err "Invalid size"
        else if | m == cmd_DEBUG -> Result.map ReceivedDebug (toByteString size payload)
                | m == cmd_PING  -> if size == 4
                                    then Result.map ReceivedPing (toByteString 4 payload)
                                    else Err "Invalid data size for 'ping request'"
                | m == cmd_VERSION ->
                    let flashSize =
                        Result.map (\b -> {flashMemSize = b})
                        <| toByte (List.head payload)
                        mpVersion mpv =
                            Result.map (\s -> {mpv | version = s})
                            -- (size - 3) because of null-termination
                                <| toByteString (size - 3) (List.tail payload)
                    in Result.map ReceivedGetVersion (flashSize `andThen` mpVersion)
                | m == cmd_CONTEXT -> if size /= 1
                        then Err "Invalid data size for 'set context'"
                        else case List.head payload of
                                0x00 -> Ok <| ReceivedSetContext UnknownContext
                                0x01 -> Ok <| ReceivedSetContext ContextSet
                                0x03 -> Ok <| ReceivedSetContext NoCardForContext
                                _    -> Err "Invalid data for 'set context'"
                | m == cmd_GET_LOGIN    -> maybeByteStringNull ReceivedGetLogin    "get login"
                | m == cmd_GET_PASSWORD -> maybeByteStringNull ReceivedGetPassword "get password"
                | m == cmd_SET_LOGIN    -> doneOrNotDone ReceivedSetLogin      "set login"
                | m == cmd_SET_PASSWORD -> doneOrNotDone ReceivedSetPassword   "set password"
                | m == cmd_CHECK_PASSWORD -> if size /= 1
                        then Err "Invalid data size for 'check password'"
                        else case List.head payload of
                            0x00 -> Ok <| ReceivedCheckPassword Incorrect
                            0x01 -> Ok <| ReceivedCheckPassword Correct
                            0x02 -> Ok <| ReceivedCheckPassword RequestBlocked
                            _    -> Err "Invalid data for 'check password'"
                | m == cmd_ADD_CONTEXT         -> doneOrNotDone ReceivedAddContext "add context"
                | m == cmd_EXPORT_FLASH        -> Result.map ReceivedExportFlash (toByteString size payload)
                | m == cmd_EXPORT_FLASH_END    -> Ok ReceivedExportFlashEnd
                | m == cmd_IMPORT_FLASH_BEGIN  -> doneOrNotDone ReceivedImportFlashStart "import flash start"
                | m == cmd_IMPORT_FLASH        -> doneOrNotDone ReceivedImportFlash      "import flash"
                | m == cmd_IMPORT_FLASH_END    -> doneOrNotDone ReceivedImportFlashEnd   "import flash end"
                | m == cmd_EXPORT_EEPROM       -> Result.map ReceivedExportEeprom (toByteString size payload)
                | m == cmd_EXPORT_EEPROM_END   -> Ok ReceivedExportEepromEnd
                | m == cmd_IMPORT_EEPROM_BEGIN -> doneOrNotDone ReceivedImportEepromStart "import eeprom start"
                | m == cmd_IMPORT_EEPROM       -> doneOrNotDone ReceivedImportEeprom      "import eeprom"
                | m == cmd_IMPORT_EEPROM_END   -> doneOrNotDone ReceivedImportEepromEnd   "import eeprom end"
                | m == cmd_ERASE_EEPROM        -> Err "Got ReceivedEraseEeprom"
                | m == cmd_ERASE_FLASH         -> Err "Got ReceivedEraseFlash"
                | m == cmd_ERASE_SMC           -> Err "Got ReceivedEraseSmc"
                | m == cmd_DRAW_BITMAP         -> Err "Got ReceivedDrawBitmap"
                | m == cmd_SET_FONT            -> Err "Got ReceivedSetFont"
                | m == cmd_EXPORT_FLASH_START  -> doneOrNotDone ReceivedExportFlashStart  "export flash start"
                | m == cmd_EXPORT_EEPROM_START -> doneOrNotDone ReceivedExportEepromStart "export eeprom start"
                | m == cmd_SET_BOOTLOADER_PWD  -> Err "Got ReceivedSetBootloaderPwd"
                | m == cmd_JUMP_TO_BOOTLOADER  -> Err "Got ReceivedJumpToBootloader"
                | m == cmd_CLONE_SMARTCARD     -> Err "Got ReceivedCloneSmartcard"
                | m == cmd_STACK_FREE          -> Err "Got ReceivedStackFree"
                | m == cmd_GET_RANDOM_NUMBER   -> Result.map ReceivedGetRandomNumber (toByteString size payload)
                | m == cmd_START_MEMORYMGMT    -> doneOrNotDone ReceivedManageModeStart  "start memory management mode"
                | m == cmd_END_MEMORYMGMT      -> doneOrNotDone ReceivedManageModeEnd    "end memory management mode"
                | m == cmd_IMPORT_MEDIA_START  -> doneOrNotDone ReceivedImportMediaStart "media import start"
                | m == cmd_IMPORT_MEDIA        -> doneOrNotDone ReceivedImportMedia      "media import"
                | m == cmd_IMPORT_MEDIA_END    -> doneOrNotDone ReceivedImportMediaEnd   "media import end"
                | m == cmd_READ_FLASH_NODE     -> Result.map ReceivedReadFlashNode (toByteArray size payload)
                | m == cmd_WRITE_FLASH_NODE    -> doneOrNotDone ReceivedWriteFlashNode    "write node in flash"
                | m == cmd_SET_FAVORITE        -> doneOrNotDone ReceivedSetFavorite       "set favorite"
                | m == cmd_SET_STARTING_PARENT -> doneOrNotDone ReceivedSetStartingParent "set starting parent"
                | m == cmd_SET_CTRVALUE        -> doneOrNotDone ReceivedSetCtrValue       "set CTR value"
                | m == cmd_ADD_CARD_CPZ_CTR    -> doneOrNotDone ReceivedAddCpzCtr         "set CPZ CTR value"
                | m == cmd_GET_CARD_CPZ_CTR    -> maybeByteString ReceivedGetCpzCtrValues "get CPZ CTR value"
                | m == cmd_CARD_CPZ_CTR_PACKET ->
                    let cpz  =
                            Result.map (\c -> {cpz = c})
                                <| toByteString 8 payload
                        ctrNonce d =
                            Result.map (\s -> {d | ctrNonce = s})
                                <| toByteString 16 (List.drop 8 payload)
                    in Result.map ReceivedCpzCtrPacketExport (cpz `andThen` ctrNonce)
                | m == cmd_SET_MOOLTIPASS_PARM -> doneOrNotDone ReceivedSetParameter "set Mooltipass parameter"
                | m == cmd_GET_MOOLTIPASS_PARM -> maybeByteString ReceivedGetParameter    "get parameter"
                | m == cmd_GET_FAVORITE -> if size == 4 then case payload of
                            (addrP1::addrP2::addrC1::addrC2::_) ->
                                let p = (addrP1,addrP2)
                                    c = (addrC1,addrC2)
                                in Ok <| ReceivedGetFavorite (p,c)
                            _ -> Err "Invalid data for get favorite"
                        else Err "Invalid data for get favorite"
                | m == cmd_RESET_CARD      -> doneOrNotDone ReceivedResetCard         "reset card"
                | m == cmd_READ_CARD_LOGIN -> maybeByteStringNull ReceivedGetCardLogin    "get card login"
                | m == cmd_READ_CARD_PASS  -> maybeByteStringNull ReceivedGetCardPassword "get card password"
                | m == cmd_SET_CARD_LOGIN  -> doneOrNotDone ReceivedSetCardLogin      "set card password"
                | m == cmd_SET_CARD_PASS   -> doneOrNotDone ReceivedSetCardPassword   "set card password"
                | m == cmd_GET_STARTING_PARENT ->
                    if size /= 2 then Err "Invalid size for starting parent"
                    else case payload of
                        (addr1::addr2::_) -> Ok <| ReceivedGetStartingParent (addr1,addr2)
                        _ -> Err "Invalid data for starting parent"
                | m == cmd_GET_CTRVALUE       -> maybeByteString ReceivedGetCtrValue       "get CTR value"
                | m == cmd_ADD_UNKNOWN_CARD   -> doneOrNotDone ReceivedAddNewCard "add unknown smartcard"
                | m == cmd_USB_KEYBOARD_PRESS -> Err "Got ReceivedUsbKeyboardPress"
                | m == cmd_MOOLTIPASS_STATUS  -> Err "ReceivedGetStatus"
                | otherwise -> Err <| "Got unknown message: " ++ toString m
