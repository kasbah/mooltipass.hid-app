module DeviceFlash where

-- local source
import Byte (..)
import DevicePacket (..)

type alias ParentNode =
    { prevParentAddress : FlashAddress
    , nextParentAddress : FlashAddress
    , nextChildAddress  : FlashAddress
    , service           : ByteString
    }

type alias ChildNode =
    { nextChildAddress : FlashAddress
    , prevChildAddress : FlashAddress
    , ctr              : (Byte, Byte, Byte)
    , description      : ByteString
    , login            : ByteString
    , password         : ByteArray
    , dateCreated      : (Byte, Byte)
    , dateLastUsed     : (Byte, Byte)
    }

type alias Favorite =
    { slotNumber : Byte
    , parentNode : FlashAddress
    , childNode  : FlashAddress
    }
