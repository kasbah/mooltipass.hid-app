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

{-| The type of packets we send from the app to the device -}
type AppPacket =
     AppDebug              ByteString
   | AppPing
   | AppGetVersion
   | AppSetContext         ByteString
   | AppGetLogin
   | AppGetPassword
   | AppSetLogin           ByteString
   | AppSetPassword        ByteString
   | AppCheckPassword
   | AppAddContext         ByteString
   | AppExportFlashStart
   | AppExportFlash
   | AppExportFlashEnd
   | AppImportFlashStart   FlashSpace
   | AppImportFlash        ByteString
   | AppImportFlashEnd
   | AppExportEepromStart
   | AppExportEeprom
   | AppExportEepromEnd
   | AppImportEepromStart
   | AppImportEeprom       ByteString
   | AppImportEepromEnd
   | AppGetRandomNumber
   | AppMemoryManageModeStart
   | AppMemoryManageModeEnd
   | AppImportMediaStart
   | AppImportMedia        ByteString
   | AppImportMediaEnd
   | AppReadFlashNode      FlashAddress
   | AppWriteFlashNode     FlashAddress Byte ByteString
   | AppSetFavorite        Byte FlashAddress FlashAddress ByteString
   | AppSetStartingParent  FlashAddress
   -- CPZ = code protected zone
   -- CTR = counter value for Eeprom
   | AppSetCtrValue        (Byte, Byte, Byte)
   | AppAddCpzCtr          CpzCtrLutEntry
   | AppGetCpzCtrValues
   | AppSetParameter       Parameter Byte
   | AppGetParameter       Parameter
   | AppGetFavorite        Byte
   | AppResetCard          (Byte, Byte)
   | AppGetCardLogin
   | AppGetCardPassword
   | AppSetCardLogin       ByteString
   | AppSetCardPassword    ByteString
   | AppGetFreeSlotAddress
   | AppGetStartingParent
   | AppGetCtrValue
   | AppAddNewCard
   | AppGetStatus
-- disabled developer types:
    --AppEraseEeprom      -> 0x40
    --AppEraseFlash       -> 0x41
    --AppEraseSmc         -> 0x42
    --AppDrawBitmap       -> 0x43
    --AppSetFont          -> 0x44
    --AppSetBootloaderPwd -> 0x47
    --AppJumpToBootloader -> 0x48
    --AppCloneSmartcard   -> 0x49
    --AppStackFree        -> 0x4A
    --AppUsbKeyboardPress -> 0x69


{-| The type of packets we receive from the device -}
type DevicePacket =
      DeviceDebug             ByteString
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
    | DeviceAddCpzCtr         ReturnCode
    | DeviceGetCpzCtrValues   (Maybe ByteString)
    | DeviceCpzCtrPacketExport CpzCtrLutEntry
    | DeviceSetParameter      ReturnCode
    | DeviceGetParameter      (Maybe ByteString)
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
    | DeviceGetStatus         Status

{-| Carries firmware version and flash memory size -}
type alias MpVersion = { flashMemSize : Byte
                       , version : ByteString
                       }

{-| Code protected zone look-up-table entry -}
type alias CpzCtrLutEntry = { cpz : ByteString
                            , ctrNonce : ByteString
                            }

{-| Return for 'DeviceCheckPassword' -}
type CheckPasswordReturn = Incorrect | Correct | RequestBlocked

{-| Return for 'DeviceSetContext' -}
type SetContextReturn = UnknownContext | ContextSet | NoCardForContext

{-| Return for 'DeviceGetStatus' -}
type Status = NeedCard | Locked | LockScreen | Unlocked

{-| This is (LSB, MSB) -}
type alias FlashAddress = (Byte, Byte)

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
toInts : AppPacket -> List Int
toInts msg =
    -- the packet format is [payload-size, message-type, payload ... ]
    let byteString msgType s = String.length s::msgType::stringToInts s
        byteStringNull msgType s =  (String.length s + 1)::msgType::stringToInts s ++ [0]
        zeroSize msgType     = [0, msgType]
        stringToInts s       = List.map Char.toCode (String.toList s)
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
        AppDebug       s  -> byteString 0x01 s
        AppPing           -> zeroSize 0x02
        AppGetVersion     -> zeroSize 0x03
        AppSetContext  s  -> byteStringNull 0x04 s
        AppGetLogin       -> zeroSize 0x05
        AppGetPassword    -> zeroSize 0x06
        AppSetLogin    s  -> byteString 0x07 s
        AppSetPassword s  -> byteString 0x08 s
        AppCheckPassword  -> zeroSize 0x09
        AppAddContext  s  -> byteString 0x0A s
        AppExportFlash    -> zeroSize 0x30
        AppExportFlashEnd -> zeroSize 0x31
        AppImportFlashStart space -> [ 1
                                     , 0x32
                                     , case space of
                                         FlashUserSpace     -> 0x00
                                         FlashGraphicsSpace -> 0x01
                                     ]
        AppImportFlash  s        -> byteString 0x33 s
        AppImportFlashEnd        -> zeroSize 0x34
        AppExportEeprom          -> zeroSize 0x35
        AppExportEepromEnd       -> zeroSize 0x36
        AppImportEepromStart     -> zeroSize 0x37
        AppImportEeprom s        -> byteString 0x38 s
        AppImportEepromEnd       -> zeroSize 0x39
        AppExportFlashStart      -> zeroSize 0x45
        AppExportEepromStart     -> zeroSize 0x46
        AppGetRandomNumber       -> zeroSize 0x4B
        AppMemoryManageModeStart -> zeroSize 0x50
        AppMemoryManageModeEnd   -> zeroSize 0x51
        AppImportMediaStart      -> zeroSize 0x52
        AppImportMedia  s        -> byteString 0x53 s
        AppImportMediaEnd        -> zeroSize 0x54
        AppReadFlashNode (a1,a2) -> [2, 0x55, a1, a2]
        AppWriteFlashNode (a1,a2) n s       ->
            (String.length s + 3)::0x56::a1::a2::n::stringToInts s
        AppSetFavorite id (p1,p2) (c1,c2) s ->
            (String.length s + 5)::0x57::id::p1::p2::c1::c2::stringToInts s
        AppSetStartingParent (a1,a2)    -> [2, 0x58, a1, a2]
        AppSetCtrValue (ctr1,ctr2,ctr3) -> [3, 0x59, ctr1, ctr2, ctr3]
        AppAddCpzCtr c -> 24::0x5A::stringToInts c.cpz ++ stringToInts c.ctrNonce
        AppGetCpzCtrValues    -> zeroSize 0x5B
        AppSetParameter p b   -> [2, 0x5D, param p, b]
        AppGetParameter p     -> [1, 0x5E, param p]
        AppGetFavorite  b     -> [1, 0x5F, b]
        AppResetCard (b1,b2)  -> [2, 0x60, b1, b2]
        AppGetCardLogin       -> zeroSize 0x61
        AppGetCardPassword    -> zeroSize 0x62
        AppSetCardLogin s     -> byteString 0x63 s
        AppSetCardPassword s  -> byteString 0x64 s
        AppGetFreeSlotAddress -> zeroSize 0x65
        AppGetStartingParent  -> zeroSize 0x66
        AppGetCtrValue        -> zeroSize 0x67
        AppAddNewCard         -> zeroSize 0x68
        AppGetStatus          -> zeroSize 0x70

{-| Convert a list of ints received through a port from chrome.hid.receive into
    a packet we can interpret -}
fromInts : List Int -> Result Error DevicePacket
fromInts (size::messageType::payload) =
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
        else case messageType of
            0x01 -> Result.map DeviceDebug (toByteString size payload)
            0x02 -> if size == 4
                    then Result.map DevicePing (toByteString 4 payload)
                    else Err "Invalid data size for 'ping request'"
            0x03 -> let flashSize =
                            Result.map (\b -> {flashMemSize = b})
                                <| toByte (List.head payload)
                        mpVersion mpv =
                            Result.map (\s -> {mpv | version = s})
                                <| toByteString (size - 1) (List.tail payload)
                    in Result.map DeviceGetVersion (flashSize `andThen` mpVersion)
            0x04 -> if size /= 1
                    then Err "Invalid data size for 'set context'"
                    else case List.head payload of
                        0x00 -> Ok <| DeviceSetContext UnknownContext
                        0x01 -> Ok <| DeviceSetContext ContextSet
                        0x03 -> Ok <| DeviceSetContext NoCardForContext
                        _    -> Err "Invalid data for 'set context'"
            0x05 -> maybeByteStringNull DeviceGetLogin    "get login"
            0x06 -> maybeByteStringNull DeviceGetPassword "get password"
            0x07 -> doneOrNotDone DeviceSetLogin      "set login"
            0x08 -> doneOrNotDone DeviceSetPassword   "set password"
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
            0x40 -> Err "Got DeviceEraseEeprom"
            0x41 -> Err "Got DeviceEraseFlash"
            0x42 -> Err "Got DeviceEraseSmc"
            0x43 -> Err "Got DeviceDrawBitmap"
            0x44 -> Err "Got DeviceSetFont"
            0x45 -> doneOrNotDone DeviceExportFlashStart  "export flash start"
            0x46 -> doneOrNotDone DeviceExportEepromStart "export eeprom start"
            0x47 -> Err "Got DeviceSetBootloaderPwd"
            0x48 -> Err "Got DeviceJumpToBootloader"
            0x49 -> Err "Got DeviceCloneSmartcard"
            0x4A -> Err "Got DeviceStackFree"
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
            0x5A -> doneOrNotDone DeviceAddCpzCtr         "set CPZ CTR value"
            0x5B -> maybeByteString DeviceGetCpzCtrValues "get CPZ CTR value"
            0x5C -> let cpz  =
                            Result.map (\c -> {cpz = c})
                                <| toByteString 8 payload
                        ctrNonce d =
                            Result.map (\s -> {d | ctrNonce = s})
                                <| toByteString 16 (List.drop 8 payload)
                    in Result.map DeviceCpzCtrPacketExport (cpz `andThen` ctrNonce)
            0x5D -> doneOrNotDone DeviceSetParameter "set Mooltipass parameter"
            0x5E -> maybeByteString DeviceGetParameter    "get parameter"
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
            0x69 -> Err "Got DeviceUsbKeyboardPress"
            0x70 -> if size /= 1
                    then Err "Invalid data size for 'get status'"
                    else case (List.head payload) `and` 0x7 of
                                0x0 -> Ok <| DeviceGetStatus NeedCard
                                0x1 -> Ok <| DeviceGetStatus Locked
                                0x3 -> Ok <| DeviceGetStatus LockScreen
                                0x5 -> Ok <| DeviceGetStatus Unlocked
                                _   -> Err "Invalid status received in 'get status'"
            _    -> Err <| "Got unknown message: " ++ toString messageType


{-| A byte is just an Int really. -}
type alias Byte = Int

{-| We make sure values are between 0 and 255 when we convert to a byte. -}
toByte : Int -> Result Error Byte
toByte x = if (x >= 0) && (x < 256)
           then Ok x
           else Err "Invalid int given to byte conversion"

{-| A bytestring is just a string really -}
type alias ByteString = String

{-| We make sure the length (size) is right and values are between 0 and 255
    when we convert to a bytestring. -}
toByteString : Int -> List Int -> Result Error ByteString
toByteString size payload =
    if size > List.length payload || size <= 0
    then Err "Invalid size to convert to bytestring"
    else if List.foldr (\int b -> b && int >= 0 && int < 256) True
                (List.take size payload)
         then Ok <| String.fromList (List.map Char.fromCode (List.take size payload))
         else Err "Invalid char given to byte conversion (unicode?)"

{-| Our error type is just a string that explains the error. -}
type alias Error = String
