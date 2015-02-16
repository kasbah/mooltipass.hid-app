module DeviceFlash where

import List (..)

-- local source
import Byte (..)
import DevicePacket (..)
import CommonState (..)
import Util (..)

type ParentNode =
    ParentNode
        { address    : FlashAddress
        , prevParent : ParentNode
        , nextParent : ParentNode
        , nextChild  : ChildNode
        , service    : ByteString
        }
    | EmptyParentNode

type ChildNode =
    ChildNode
        { address      : FlashAddress
        , nextChild    : ChildNode
        , prevChild    : ChildNode
        , ctr          : (Byte, Byte, Byte)
        , description  : ByteString
        , login        : ByteString
        , password     : ByteArray
        , dateCreated  : (Byte, Byte)
        , dateLastUsed : (Byte, Byte)
        }
    | EmptyChildNode

type alias FavoriteSlot =
    { slotNumber : Byte
    , parentNode : ParentNode
    , childNode  : ChildNode
    }

foldChildren' : (ChildNode -> a -> a) -> a -> ChildNode -> a
foldChildren' f z n = case n of
    ChildNode n'   -> f n (foldChildren' f z n'.nextChild)
    EmptyChildNode -> z

foldChildren : (ChildNode -> a -> a) -> a -> ParentNode -> a
foldChildren f z n = case n of
    ParentNode n' -> foldChildren' f z n'.nextChild
    EmptyParentNode -> z

foldParents : (ParentNode -> a -> a) -> a -> ParentNode -> a
foldParents f z n = case n of
    ParentNode n'   -> f n (foldParents f z n'.nextParent)
    EmptyParentNode -> z

--type alias MemoryInfo =
--    { credentials : List (String, List String)
--    , favorites   : List (Maybe (String, String))
--    }

getCreds : ParentNode -> List (String, List String)
getCreds firstParent =
    let getLogins parent = foldChildren (\(ChildNode c) z -> c.login::z) [] parent
    in foldParents (\(ParentNode p) z -> (p.service, getLogins (ParentNode p))::z) [] firstParent

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

