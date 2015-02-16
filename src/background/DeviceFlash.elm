module DeviceFlash where

import List (..)

-- local source
import Byte (..)
import DevicePacket (..)
import CommonState (..)
import Util (..)

type alias DeviceFlash =
    { parentNodes : List ParentNode
    , favorites   : List FavoriteSlot
    }

type Node = NodeP ParentNode | NodeC ChildNode

type ParentNode =
    ParentNode
        { address    : FlashAddress
        , prevParent : Maybe ParentNode
        , nextParent : Maybe ParentNode
        , nextChild  : Maybe ChildNode
        , service    : ByteString
        }

type ChildNode =
    ChildNode
        { address      : FlashAddress
        , nextChild    : Maybe ChildNode
        , prevChild    : Maybe ChildNode
        , ctr          : (Byte, Byte, Byte)
        , description  : ByteString
        , login        : ByteString
        , password     : ByteArray
        , dateCreated  : (Byte, Byte)
        , dateLastUsed : (Byte, Byte)
        }

type alias FavoriteSlot =
    { slotNumber : Byte
    , parentNode : ParentNode
    , childNode  : ChildNode
    }

--foldChildren : ({a | nextChild : Maybe ChildNode} -> b -> b) -> b -> {a | nextChild : Maybe ChildNode} -> b
foldChildren f z n = if n.nextChild /= Nothing then f n (foldChildren (fromJust n.nextChild)) else f n z


--foldNode : (Node -> a -> a) -> a -> Node -> a
--foldNode f z n =
--    let foldNode' z' n'' = case n''.nextChild of
--        Nothing -> f n'' z'
--        Just nC -> f (foldNode f z' n'') z'
--    in case n of
--        NodeP n' -> foldNode' z n'
--        NodeC n' -> foldNode' z n'



null : FlashAddress
null = (0,0)
--
--toInfo : DeviceFlash -> MemoryInfo
--toInfo fl =
--    let getChildren : ParentNode -> List ChildNode
--        getChildren p =
--    in map getChildren fl.parentNodes


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

