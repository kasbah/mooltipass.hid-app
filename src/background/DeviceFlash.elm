module DeviceFlash where

import List (..)

-- local source
import Byte (..)
import DevicePacket (..)
import CommonState (..)

type alias DeviceFlash =
    { parentNodes : List ParentNode
    , childNodes  : List ChildNode
    , favorites   : List Favorite
    }

type alias ParentNode =
    { address           : FlashAddress
    , prevParentAddress : FlashAddress
    , nextParentAddress : FlashAddress
    , nextChildAddress  : FlashAddress
    , service           : ByteString
    }

type alias ChildNode =
    { address          : FlashAddress
    , nextChildAddress : FlashAddress
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

null : FlashAddress
null = (0,0)

--toInfo : DeviceFlash -> MemoryInfo
--toInfo fl =
--    let clearChildLess ns =
--            foldl
--                (\n z -> if n.nextChildAddress == null then z else n::z)
--                []
--                ns
--        getService n = (n.service, n.nextChildAddress)
--        services     = map getService (clearChildLess fl.parentNodes)
--        getNextChild cAdd ns =
--            if cAdd == null then Nothing
--            else foldr
--                (\n z -> if n.address == cAdd then Just n else z)
--                Nothing
--                ns
--        getChildren firstAdd = foldr (\_ (a, matched, unmatched) -> if (head unmatched).address == a then
--
--    in {credentials  = map services
--
--type alias MemoryInfo =
--    { credentials : List (String, List String)
--    , favorites   : List (Maybe (String, String))
--    }

