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
                | AppGetStartingParentAddress
                | AppGetCtrValue
                | AppAddNewCard

type ReturnByteString = ByteString ByteString | NotPerformedS
type ReturnCode = Performed | NotPerformed

type DeviceMessage = DeviceDebug            ByteString
                   | DevicePing             ByteString
                   | DeviceGetVersion       MpVersion
                   | DeviceSetContext       SetContextReturn
                   | DeviceGetLogin         ReturnByteString
                   | DeviceGetPassword      ReturnByteString
                   | DeviceSetLogin         Return
                   | DeviceSetPassword      Return
                   | DeviceCheckPassword    CheckPasswordReturn
                   | DeviceAddContext
                   | DeviceExportFlashStart
                   | DeviceExportFlash      ByteString
                   | DeviceExportFlashEnd
                   | DeviceImportFlashStart
                   | DeviceImportFlash
                   | DeviceImportFlashEnd
                   | DeviceExportEepromStart
                   | DeviceExportEeprom     ByteString
                   | DeviceExportEepromEnd
                   | DeviceImportEepromStart
                   | DeviceImportEeprom
                   | DeviceImportEepromEnd
                   | DeviceGetRandomNumber  ByteString
                   | DeviceMemoryManageModeStart
                   | DeviceMemoryManageModeEnd
                   | DeviceImportMediaStart
                   | DeviceImportMediaEnd
                   | DeviceImportMedia
                   | DeviceReadFlashNode  ByteString
                   | DeviceWriteFlashNode
                   | DeviceSetFavorite
                   | DeviceSetStartingParent
                   | DeviceSetCtrValue
                   | DeviceAddCpzCtrValue
                   | DeviceGetCpzCtrValues
                   | DeviceCpzCtrPacketExport ByteString  ByteString
                   | DeviceSetParameter
                   | DeviceGetParameter  ByteString
                   | DeviceGetFavorite
                   | DeviceResetCard
                   | DeviceGetCardLogin  ByteString
                   | DeviceGetCardPassword  ByteString
                   | DeviceSetCardLogin
                   | DeviceSetCardPassword
                   | DeviceGetStartingParentAddress  ByteString
                   | DeviceGetCtrValue  (Byte,Byte,Byte)
                   | DeviceAddNewCard

type alias MpVersion = { flashMemSize : Byte
                       , version : ByteString
                       }
defaultMpVersion = { flashMemSize = '\0'
                   , version = "???"
                   }

type CheckPasswordReturn = Incorrect | Correct | RequestBlocked
type SetContextReturn = UnknownContext | ContextSet | NoCardForContext

fromBytes : List Int -> Result Error DeviceMessage
fromBytes (size::messageType::payload) =
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
        0x05 -> if size <= 0
                then Err "Zero data returned for 'get login'"
                else if size == 1 && List.head payload == 0x00
                     then Ok <| DeviceGetLogin NotPerformed
                     else Result.map (DeviceGetLogin << Performed) (toByteString size payload)
        0x06 -> if size <= 0
                then Err "Zero data returned for 'get password'"
                else if size == 1 && List.head payload == 0x00
                     then Ok <| DeviceGetPassword NotPerformed
                     else Result.map (DeviceGetPassword << Performed) (toByteString size payload)
        0x07 -> if size /= 1
                then Err "Invalid data size for 'set login'"
                else case List.head payload of
                    0x00 -> Ok <| DeviceSetLogin NotPerformed
                    0x01 -> Ok <| DeviceSetLogin <| Performed ""
                    _    -> Err "Invalid data for 'set login'"
        0x08 -> if size /= 1
                then Err "Invalid data size for 'set password'"
                else case List.head payload of
                    0x00 -> Ok <| DeviceSetPassword NotPerformed
                    0x01 -> Ok <| DeviceSetPassword <| Performed ""
                    _    -> Err "Invalid data for 'set password'"
        0x09 -> if size /= 1
                then Err "Invalid data size for 'check password'"
                else case List.head payload of
                    0x00 -> Ok <| DeviceCheckPassword Incorrect
                    0x01 -> Ok <| DeviceCheckPassword Correct
                    0x02 -> Ok <| DeviceCheckPassword RequestBlocked
                    _    -> Err "Invalid data for 'check password'"
        --0x0A -> DeviceAddContext
        --0x30 -> DeviceExportFlash
        --0x31 -> DeviceExportFlashEnd
        --0x32 -> DeviceImportFlashBegin
        --0x33 -> DeviceImportFlash
        --0x34 -> DeviceImportFlashEnd
        --0x35 -> DeviceExportEeprom
        --0x36 -> DeviceExportEepromEnd
        --0x37 -> DeviceImportEepromBegin
        --0x38 -> DeviceImportEeprom
        --0x39 -> DeviceImportEepromEnd
        --0x40 -> DeviceEraseEeprom
        --0x41 -> DeviceEraseFlash
        --0x42 -> DeviceEraseSmc
        --0x43 -> DeviceDrawBitmap
        --0x44 -> DeviceSetFont
        --0x45 -> DeviceExportFlashStart
        --0x46 -> DeviceExportEepromStart
        --0x47 -> DeviceSetBootloaderPwd
        --0x48 -> DeviceJumpToBootloader
        --0x49 -> DeviceCloneSmartcard
        --0x4A -> DeviceStackFree
        --0x4B -> DeviceGetRandomNumber
        --0x50 -> DeviceStartMemorymgmt
        --0x51 -> DeviceEndMemorymgmt
        --0x52 -> DeviceImportMediaStart
        --0x53 -> DeviceImportMedia
        --0x54 -> DeviceImportMediaEnd
        --0x55 -> DeviceReadFlashNode
        --0x56 -> DeviceWriteFlashNode
        --0x57 -> DeviceSetFavorite
        --0x58 -> DeviceSetStartingparent
        --0x59 -> DeviceSetCtrvalue
        --0x5A -> DeviceAddCardCpzCtr
        --0x5B -> DeviceGetCardCpzCtr
        --0x5C -> DeviceCardCpzCtrPacket
        --0x5D -> DeviceSetMooltipassParm
        --0x5E -> DeviceGetMooltipassParm
        --0x5F -> DeviceGetFavorite
        --0x60 -> DeviceResetCard
        --0x61 -> DeviceReadCardLogin
        --0x62 -> DeviceReadCardPass
        --0x63 -> DeviceSetCardLogin
        --0x64 -> DeviceSetCardPass
        --0x65 -> DeviceGetFreeSlotAddr
        --0x66 -> DeviceGetStartingParent
        --0x67 -> DeviceGetCtrvalue
        --0x68 -> DeviceAddUnknownCard
        --0x69 -> DeviceUsbKeyboardPress

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
