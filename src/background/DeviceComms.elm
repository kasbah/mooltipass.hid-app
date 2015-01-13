import Result (..)
import Result
import List
import List ((::))
import Maybe
import String
import Char
type AppMessage = AppDebug ByteString
                | AppPing (Byte, Byte, Byte, Byte)
                | AppGetVersion
                | AppSetContext ByteString
                | AppGetLogin
                | AppGetPassword
                | AppSetLogin ByteString
                | AppSetPassword ByteString
                | AppCheckPassword
                | AppAddContext ByteString
                | AppExportFlashStart
                | AppExportFlash
                | AppExportFlashEnd
                | AppImportFlashStart FlashSpace
                | AppImportFlash ByteString
                | AppImportFlashEnd
                | AppExportEepromStart
                | AppExportEeprom
                | AppExportEepromEnd
                | AppImportEepromStart
                | AppImportEeprom ByteString
                | AppImportEepromEnd
                | AppGetRandomNumber
                | AppMemoryManageModeStart
                | AppMemoryManageModeEnd
                | AppImportMediaStart
                | AppImportMedia ByteString
                | AppImportMediaEnd
                | AppReadFlashNode (Byte, Byte)
                | AppSetFavorite (Byte, Byte, Byte, Byte, Byte)
                | AppSetStartingParent (Byte, Byte)
                | AppSetCtrValue (Byte, Byte, Byte)
                | AppAddCpzCtrValue ByteString ByteString
                | AppGetCpzCtrValues
                | AppCpzCtrPacketExport
                | AppSetParameter Parameter Byte
                | AppGetParameter Parameter
                | AppGetFavorite Byte
                | AppResetCard (Byte, Byte)
                | AppGetCardLogin
                | AppGetCardPassword
                | AppSetCardLogin ByteString
                | AppSetCardPassword ByteString
                | AppGetFreeSlotAddress
                | AppGetStartingParent
                | AppGetCtrValue
                | AppAddNewCard

type ReturnCode = Done | NotDone

type DeviceMessage = DeviceDebug             ByteString
                   | DevicePing              ByteString
                   | DeviceGetVersion        MpVersion
                   | DeviceSetContext        SetContextReturn
                   | DeviceGetLogin          (Maybe ByteString)
                   | DeviceGetPassword       (Maybe ByteString)
                   | DeviceSetLogin          ReturnCode
                   | DeviceSetPassword       ReturnCode
                   | DeviceCheckPassword     CheckPasswordReturn
                   | DeviceAddContext        ReturnCode
                   | DeviceExportFlashStart  ReturnCode
                   | DeviceExportFlash       ByteString
                   | DeviceExportFlashEnd
                   | DeviceImportFlashStart  ReturnCode
                   | DeviceImportFlash       ReturnCode
                   | DeviceImportFlashEnd    ReturnCode
                   | DeviceExportEepromStart ReturnCode
                   | DeviceExportEeprom      ByteString
                   | DeviceExportEepromEnd
                   | DeviceImportEepromStart ReturnCode
                   | DeviceImportEeprom      ReturnCode
                   | DeviceImportEepromEnd   ReturnCode
                   | DeviceGetRandomNumber   ByteString
                   | DeviceManageModeStart   ReturnCode
                   | DeviceManageModeEnd     ReturnCode
                   | DeviceImportMediaStart  ReturnCode
                   | DeviceImportMediaEnd    ReturnCode
                   | DeviceImportMedia       ReturnCode
                   | DeviceReadFlashNode     ByteString
                   | DeviceWriteFlashNode    ReturnCode
                   | DeviceSetFavorite       ReturnCode
                   | DeviceSetStartingParent ReturnCode
                   | DeviceSetCtrValue       ReturnCode
                   | DeviceAddCpzCtrValue    ReturnCode
                   | DeviceGetCpzCtrValue    (Maybe ByteString)
                   | DeviceCpzCtrPacketExport CpzCtrLutEntryPacket
                   | DeviceSetParameter      ReturnCode
                   | DeviceGetParameter      ByteString
                   | DeviceGetFavorite       (Maybe ByteString)
                   | DeviceResetCard         ReturnCode
                   | DeviceGetCardLogin      (Maybe ByteString)
                   | DeviceGetCardPassword   (Maybe ByteString)
                   | DeviceSetCardLogin      ReturnCode
                   | DeviceSetCardPassword   ReturnCode
                   | DeviceGetFreeSlotAddr   (Maybe ByteString)
                   | DeviceGetStartingParent (Maybe ByteString)
                   | DeviceGetCtrValue       (Maybe ByteString)
                   | DeviceAddNewCard        ReturnCode

type alias MpVersion = { flashMemSize : Byte
                       , version : ByteString
                       }
defaultMpVersion = { flashMemSize = '\0'
                   , version = "???"
                   }

type alias CpzCtrLutEntryPacket = { cpz : ByteString
                                  , ctrNonce : ByteString
                                  }
defaultCpzCtrLutEntryPacket = {cpz = "", ctrNonce = ""}


type CheckPasswordReturn = Incorrect | Correct | RequestBlocked
type SetContextReturn = UnknownContext | ContextSet | NoCardForContext

fromBytes : List Int -> Result Error DeviceMessage
fromBytes (size::messageType::payload) =
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
                 else Result.map (constructor << Maybe.Just) (toByteString size payload)
    in
        if size > List.length payload
        then Err "Invalid size"
        else case messageType of
            0x01 -> Result.map DeviceDebug (toByteString size payload)
            0x02 -> if size == 4
                    then Result.map DevicePing (toByteString 4 payload)
                    else Err "Invalid size for 'ping request' return."
            0x03 -> let flashSize =
                            Result.map (\b -> {defaultMpVersion | flashMemSize <- b})
                                <| toByte (List.head payload)
                        mpVersion mpv =
                            Result.map (\s -> {mpv | version <- s})
                                <| toByteString (size - 1) (List.tail payload)
                    in Result.map DeviceGetVersion (flashSize `andThen` mpVersion)
            0x04 -> if size /= 1
                    then Err "Invalid data size for 'set context'"
                    else case List.head payload of
                        0x00 -> Ok <| DeviceSetContext UnknownContext
                        0x01 -> Ok <| DeviceSetContext ContextSet
                        0x03 -> Ok <| DeviceSetContext NoCardForContext
                        _    -> Err "Invalid data for 'set context'"
            0x05 -> maybeByteString DeviceGetLogin "get login"
            0x06 -> maybeByteString DeviceGetPassword "get password"
            0x07 -> doneOrNotDone DeviceSetLogin "set login"
            0x08 -> doneOrNotDone DeviceSetPassword "set password"
            0x09 -> if size /= 1
                    then Err "Invalid data size for 'check password'"
                    else case List.head payload of
                        0x00 -> Ok <| DeviceCheckPassword Incorrect
                        0x01 -> Ok <| DeviceCheckPassword Correct
                        0x02 -> Ok <| DeviceCheckPassword RequestBlocked
                        _    -> Err "Invalid data for 'check password'"
            0x0A -> doneOrNotDone DeviceAddContext "add context"
            0x30 -> Result.map DeviceExportFlash (toByteString size payload)
            0x31 -> Ok DeviceExportFlashEnd
            0x32 -> doneOrNotDone DeviceImportFlashStart "import flash start"
            0x33 -> doneOrNotDone DeviceImportFlash      "import flash"
            0x34 -> doneOrNotDone DeviceImportFlashEnd   "import flash end"
            0x35 -> Result.map DeviceExportEeprom (toByteString size payload)
            0x36 -> Ok DeviceExportEepromEnd
            0x37 -> doneOrNotDone DeviceImportEepromStart "import eeprom start"
            0x38 -> doneOrNotDone DeviceImportEeprom      "import eeprom"
            0x39 -> doneOrNotDone DeviceImportEepromEnd   "import eeprom end"
            0x40 -> Err "got DeviceEraseEeprom"
            0x41 -> Err "got DeviceEraseFlash"
            0x42 -> Err "got DeviceEraseSmc"
            0x43 -> Err "got DeviceDrawBitmap"
            0x44 -> Err "got DeviceSetFont"
            0x45 -> doneOrNotDone DeviceExportFlashStart  "export flash start"
            0x46 -> doneOrNotDone DeviceExportEepromStart "export eeprom start"
            0x47 -> Err "got DeviceSetBootloaderPwd"
            0x48 -> Err "got DeviceJumpToBootloader"
            0x49 -> Err "got DeviceCloneSmartcard"
            0x4A -> Err "got DeviceStackFree"
            0x4B -> Result.map DeviceGetRandomNumber (toByteString size payload)
            0x50 -> doneOrNotDone DeviceManageModeStart  "start memory management mode"
            0x51 -> doneOrNotDone DeviceManageModeEnd    "end memory management mode"
            0x52 -> doneOrNotDone DeviceImportMediaStart "media import start"
            0x53 -> doneOrNotDone DeviceImportMedia      "media import"
            0x54 -> doneOrNotDone DeviceImportMediaEnd   "media import end"
            0x55 -> Result.map DeviceReadFlashNode (toByteString size payload)
            0x56 -> doneOrNotDone DeviceWriteFlashNode    "write node in flash"
            0x57 -> doneOrNotDone DeviceSetFavorite       "set favorite"
            0x58 -> doneOrNotDone DeviceSetStartingParent "set starting parent"
            0x59 -> doneOrNotDone DeviceSetCtrValue       "set CTR value"
            0x5A -> doneOrNotDone DeviceAddCpzCtrValue    "set CPZ CTR value"
            0x5B -> if size <= 0
                    then Err "Zero data returned for 'get CPZ CTR value'"
                    else if size == 1 && List.head payload == 0x00
                         then Ok <| DeviceGetCpzCtrValue Maybe.Nothing
                         else Result.map (DeviceGetCpzCtrValue << Maybe.Just) (toByteString size payload)
            0x5C -> let cpz  =
                            Result.map (\c -> {defaultCpzCtrLutEntryPacket | cpz <- c})
                                <| toByteString 8 payload
                        ctrNonce d =
                            Result.map (\s -> {d | ctrNonce <- s})
                                <| toByteString 16 (List.drop 8 payload)
                    in Result.map DeviceCpzCtrPacketExport (cpz `andThen` ctrNonce)
            0x5D -> doneOrNotDone DeviceSetParameter "set Mooltipass parameter"
            0x5E -> Result.map DeviceGetParameter (toByteString size payload)
            0x5F -> maybeByteString DeviceGetFavorite     "get favorite"
            0x60 -> doneOrNotDone DeviceResetCard         "reset card"
            0x61 -> maybeByteString DeviceGetCardLogin    "get card login"
            0x62 -> maybeByteString DeviceGetCardPassword "get card password"
            0x63 -> doneOrNotDone DeviceSetCardLogin      "set card password"
            0x64 -> doneOrNotDone DeviceSetCardPassword   "set card password"
            0x65 -> maybeByteString DeviceGetFreeSlotAddr "get free slot address"
            0x66 -> maybeByteString DeviceGetStartingParent "get starting parent address"
            0x67 -> maybeByteString DeviceGetCtrValue       "get CTR value"
            0x68 -> doneOrNotDone DeviceAddNewCard "add unknown smartcard"
            0x69 -> Err "got DeviceUsbKeyboardPress"
            _    -> Err <| "got unknown message: " ++ toString messageType

type Parameter = UserInitKey
               | KeyboardLayout
               | UserInterTimeout
               | LockTimeoutEnable
               | LockTimeout
               | TouchDi
               | TouchWheelOs
               | TouchProxOs
               | OfflineMode

type FlashSpace = FlashUserSpace | FlashGraphicsSpace

-- CPZ = code protected zone
-- CTR = counter value for Eeprom

type alias Byte = Char

toByte : Int -> Result Error Byte
toByte x = if (x > 0) && (x < 256)
           then Ok (Char.fromCode x)
           else Err "Invalid char given to byte conversion (unicode?)"

type alias ByteString = String

toByteString : Int -> List Int -> Result Error ByteString
toByteString size payload =
    if size > List.length payload && size > 0
    then Err "Invalid size to convert to bytestring"
    else if (List.foldr (\int b -> b && int > 0 && int < 256) True (List.take size payload))
         then Ok <| String.fromList (List.map Char.fromCode (List.take size payload))
         else Err "Invalid char given to byte conversion (unicode?)"

type alias Error = String
