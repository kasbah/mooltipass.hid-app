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
   | OutgoingWriteFlashNode     FlashAddress Byte ByteString
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
   | OutgoingGetFreeSlotAddress
   | OutgoingGetStartingParent
   | OutgoingGetCtrValue
   | OutgoingAddNewCard
   | OutgoingGetStatus
-- disabled developer types:
    --OutgoingEraseEeprom      -> 0x40
    --OutgoingEraseFlash       -> 0x41
    --OutgoingEraseSmc         -> 0x42
    --OutgoingDrawBitmap       -> 0x43
    --OutgoingSetFont          -> 0x44
    --OutgoingSetBootloaderPwd -> 0x47
    --OutgoingJumpToBootloader -> 0x48
    --OutgoingCloneSmartcard   -> 0x49
    --OutgoingStackFree        -> 0x4A
    --OutgoingUsbKeyboardPress -> 0x69


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
    | ReceivedGetFavorite       (Maybe ByteString)
    | ReceivedResetCard         ReturnCode
    | ReceivedGetCardLogin      (Maybe ByteString)
    | ReceivedGetCardPassword   (Maybe ByteString)
    | ReceivedSetCardLogin      ReturnCode
    | ReceivedSetCardPassword   ReturnCode
    | ReceivedGetFreeSlotAddr   (Maybe ByteString)
    | ReceivedGetStartingParent (Maybe ByteString)
    | ReceivedGetCtrValue       (Maybe ByteString)
    | ReceivedAddNewCard        ReturnCode
    | ReceivedGetStatus         Status

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

{-| Return for 'ReceivedGetStatus' -}
type Status = PacketNoCard | PacketLocked | PacketLockScreen | PacketUnlocked

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
toInts : OutgoingPacket -> List Int
toInts msg =
    -- the packet format is [payload-size, message-type, payload ... ]
    let byteString msgType s = String.length s::msgType::stringToInts s
        byteArray msgType a  = List.length a::msgType::a
        byteStringNull msgType s =
            (String.length s + 1)::msgType::stringToInts s ++ [0]
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
        OutgoingDebug       s  -> byteString 0x01 s
        OutgoingPing           -> zeroSize 0x02
        OutgoingGetVersion     -> zeroSize 0x03
        OutgoingSetContext  s  -> byteStringNull 0x04 s
        OutgoingGetLogin       -> zeroSize 0x05
        OutgoingGetPassword    -> zeroSize 0x06
        OutgoingSetLogin    s  -> byteStringNull 0x07 s
        OutgoingSetPassword s  -> byteStringNull 0x08 s
        OutgoingCheckPassword  -> zeroSize 0x09
        OutgoingAddContext  s  -> byteStringNull 0x0A s
        OutgoingExportFlash    -> zeroSize 0x30
        OutgoingExportFlashEnd -> zeroSize 0x31
        OutgoingImportFlashStart space -> [ 1
                                     , 0x32
                                     , case space of
                                         FlashUserSpace     -> 0x00
                                         FlashGraphicsSpace -> 0x01
                                     ]
        OutgoingImportFlash  s        -> byteArray 0x33 s
        OutgoingImportFlashEnd        -> zeroSize 0x34
        OutgoingExportEeprom          -> zeroSize 0x35
        OutgoingExportEepromEnd       -> zeroSize 0x36
        OutgoingImportEepromStart     -> zeroSize 0x37
        OutgoingImportEeprom s        -> byteString 0x38 s
        OutgoingImportEepromEnd       -> zeroSize 0x39
        OutgoingExportFlashStart      -> zeroSize 0x45
        OutgoingExportEepromStart     -> zeroSize 0x46
        OutgoingGetRandomNumber       -> zeroSize 0x4B
        OutgoingMemManageModeStart -> zeroSize 0x50
        OutgoingMemManageModeEnd   -> zeroSize 0x51
        OutgoingImportMediaStart      -> zeroSize 0x52
        OutgoingImportMedia  a        -> byteArray 0x53 a
        OutgoingImportMediaEnd        -> zeroSize 0x54
        OutgoingReadFlashNode (a1,a2) -> [2, 0x55, a1, a2]
        OutgoingWriteFlashNode (a1,a2) n s       ->
            (String.length s + 3)::0x56::a1::a2::n::stringToInts s
        OutgoingSetFavorite (id,((p1,p2),(c1,c2))) ->
            [5, 0x57, id, p1, p2, c1, c2]
        OutgoingSetStartingParent (a1,a2)    -> [2, 0x58, a1, a2]
        OutgoingSetCtrValue (ctr1,ctr2,ctr3) -> [3, 0x59, ctr1, ctr2, ctr3]
        OutgoingAddCpzCtr c -> 24::0x5A::stringToInts c.cpz ++ stringToInts c.ctrNonce
        OutgoingGetCpzCtrValues    -> zeroSize 0x5B
        OutgoingSetParameter p b   -> [2, 0x5D, param p, b]
        OutgoingGetParameter p     -> [1, 0x5E, param p]
        OutgoingGetFavorite  b     -> [1, 0x5F, b]
        OutgoingResetCard (b1,b2)  -> [2, 0x60, b1, b2]
        OutgoingGetCardLogin       -> zeroSize 0x61
        OutgoingGetCardPassword    -> zeroSize 0x62
        OutgoingSetCardLogin s     -> byteStringNull 0x63 s
        OutgoingSetCardPassword s  -> byteStringNull 0x64 s
        OutgoingGetFreeSlotAddress -> zeroSize 0x65
        OutgoingGetStartingParent  -> zeroSize 0x66
        OutgoingGetCtrValue        -> zeroSize 0x67
        OutgoingAddNewCard         -> zeroSize 0x68
        OutgoingGetStatus          -> zeroSize 0x70

{-| Convert a list of ints received through a port from chrome.hid.receive into
    a packet we can interpret -}
fromInts : List Int -> Result Error ReceivedPacket
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
            0x01 -> Result.map ReceivedDebug (toByteString size payload)
            0x02 -> if size == 4
                    then Result.map ReceivedPing (toByteString 4 payload)
                    else Err "Invalid data size for 'ping request'"
            0x03 -> let flashSize =
                            Result.map (\b -> {flashMemSize = b})
                                <| toByte (List.head payload)
                        mpVersion mpv =
                            Result.map (\s -> {mpv | version = s})
                            -- (size - 3) because of null-termination
                                <| toByteString (size - 3) (List.tail payload)
                    in Result.map ReceivedGetVersion (flashSize `andThen` mpVersion)
            0x04 -> if size /= 1
                    then Err "Invalid data size for 'set context'"
                    else case List.head payload of
                        0x00 -> Ok <| ReceivedSetContext UnknownContext
                        0x01 -> Ok <| ReceivedSetContext ContextSet
                        0x03 -> Ok <| ReceivedSetContext NoCardForContext
                        _    -> Err "Invalid data for 'set context'"
            0x05 -> maybeByteStringNull ReceivedGetLogin    "get login"
            0x06 -> maybeByteStringNull ReceivedGetPassword "get password"
            0x07 -> doneOrNotDone ReceivedSetLogin      "set login"
            0x08 -> doneOrNotDone ReceivedSetPassword   "set password"
            0x09 -> if size /= 1
                    then Err "Invalid data size for 'check password'"
                    else case List.head payload of
                        0x00 -> Ok <| ReceivedCheckPassword Incorrect
                        0x01 -> Ok <| ReceivedCheckPassword Correct
                        0x02 -> Ok <| ReceivedCheckPassword RequestBlocked
                        _    -> Err "Invalid data for 'check password'"
            0x0A -> doneOrNotDone ReceivedAddContext "add context"
            0x30 -> Result.map ReceivedExportFlash (toByteString size payload)
            0x31 -> Ok ReceivedExportFlashEnd
            0x32 -> doneOrNotDone ReceivedImportFlashStart "import flash start"
            0x33 -> doneOrNotDone ReceivedImportFlash      "import flash"
            0x34 -> doneOrNotDone ReceivedImportFlashEnd   "import flash end"
            0x35 -> Result.map ReceivedExportEeprom (toByteString size payload)
            0x36 -> Ok ReceivedExportEepromEnd
            0x37 -> doneOrNotDone ReceivedImportEepromStart "import eeprom start"
            0x38 -> doneOrNotDone ReceivedImportEeprom      "import eeprom"
            0x39 -> doneOrNotDone ReceivedImportEepromEnd   "import eeprom end"
            0x40 -> Err "Got ReceivedEraseEeprom"
            0x41 -> Err "Got ReceivedEraseFlash"
            0x42 -> Err "Got ReceivedEraseSmc"
            0x43 -> Err "Got ReceivedDrawBitmap"
            0x44 -> Err "Got ReceivedSetFont"
            0x45 -> doneOrNotDone ReceivedExportFlashStart  "export flash start"
            0x46 -> doneOrNotDone ReceivedExportEepromStart "export eeprom start"
            0x47 -> Err "Got ReceivedSetBootloaderPwd"
            0x48 -> Err "Got ReceivedJumpToBootloader"
            0x49 -> Err "Got ReceivedCloneSmartcard"
            0x4A -> Err "Got ReceivedStackFree"
            0x4B -> Result.map ReceivedGetRandomNumber (toByteString size payload)
            0x50 -> doneOrNotDone ReceivedManageModeStart  "start memory management mode"
            0x51 -> doneOrNotDone ReceivedManageModeEnd    "end memory management mode"
            0x52 -> doneOrNotDone ReceivedImportMediaStart "media import start"
            0x53 -> doneOrNotDone ReceivedImportMedia      "media import"
            0x54 -> doneOrNotDone ReceivedImportMediaEnd   "media import end"
            0x55 -> Result.map ReceivedReadFlashNode (toByteArray size payload)
            0x56 -> doneOrNotDone ReceivedWriteFlashNode    "write node in flash"
            0x57 -> doneOrNotDone ReceivedSetFavorite       "set favorite"
            0x58 -> doneOrNotDone ReceivedSetStartingParent "set starting parent"
            0x59 -> doneOrNotDone ReceivedSetCtrValue       "set CTR value"
            0x5A -> doneOrNotDone ReceivedAddCpzCtr         "set CPZ CTR value"
            0x5B -> maybeByteString ReceivedGetCpzCtrValues "get CPZ CTR value"
            0x5C -> let cpz  =
                            Result.map (\c -> {cpz = c})
                                <| toByteString 8 payload
                        ctrNonce d =
                            Result.map (\s -> {d | ctrNonce = s})
                                <| toByteString 16 (List.drop 8 payload)
                    in Result.map ReceivedCpzCtrPacketExport (cpz `andThen` ctrNonce)
            0x5D -> doneOrNotDone ReceivedSetParameter "set Mooltipass parameter"
            0x5E -> maybeByteString ReceivedGetParameter    "get parameter"
            0x5F -> maybeByteString ReceivedGetFavorite     "get favorite"
            0x60 -> doneOrNotDone ReceivedResetCard         "reset card"
            0x61 -> maybeByteStringNull ReceivedGetCardLogin    "get card login"
            0x62 -> maybeByteStringNull ReceivedGetCardPassword "get card password"
            0x63 -> doneOrNotDone ReceivedSetCardLogin      "set card password"
            0x64 -> doneOrNotDone ReceivedSetCardPassword   "set card password"
            0x65 -> maybeByteString ReceivedGetFreeSlotAddr "get free slot address"
            0x66 -> maybeByteString ReceivedGetStartingParent "get starting parent address"
            0x67 -> maybeByteString ReceivedGetCtrValue       "get CTR value"
            0x68 -> doneOrNotDone ReceivedAddNewCard "add unknown smartcard"
            0x69 -> Err "Got ReceivedUsbKeyboardPress"
            0x70 -> if size /= 1
                    then Err "Invalid data size for 'get status'"
                    else case (List.head payload) `and` 0x7 of
                                0x0 -> Ok <| ReceivedGetStatus PacketNoCard
                                0x1 -> Ok <| ReceivedGetStatus PacketLocked
                                0x3 -> Ok <| ReceivedGetStatus PacketLockScreen
                                0x5 -> Ok <| ReceivedGetStatus PacketUnlocked
                                _   -> Err "Invalid status received in 'get status'"
            _    -> Err <| "Got unknown message: " ++ toString messageType
