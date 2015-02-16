module DeviceFlash where

import List (..)

-- local source
import Byte (..)
import DevicePacket (..)
import CommonState (..)
import Util (..)

type ParentNode = ParentNode ParentNodeData | EmptyParentNode

type alias ParentNodeData =
    { address    : FlashAddress
    , prevParent : ParentNode
    , nextParent : ParentNode
    , nextChild  : ChildNode
    , service    : ByteString
    }

type ChildNode = ChildNode ChildNodeData | EmptyChildNode

type alias ChildNodeData =
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

type alias FavoriteSlot =
    { slotNumber : Byte
    , parentNode : ParentNode
    , childNode  : ChildNode
    }

foldChildren : (ChildNodeData -> a -> a) -> a -> ChildNode -> a
foldChildren f z n = case n of
    ChildNode data -> f data (foldChildren f z data.nextChild)
    EmptyChildNode -> z

foldParents : (ParentNodeData -> a -> a) -> a -> ParentNode -> a
foldParents f z n = case n of
    ParentNode data -> f data (foldParents f z data.nextParent)
    EmptyParentNode -> z

toCreds : ParentNode -> List (String, List String)
toCreds firstParent =
    let getLogins firstChild = foldChildren (\c z -> c.login::z) [] firstChild
    in foldParents (\p z -> (p.service, getLogins p.nextChild)::z) [] firstParent
