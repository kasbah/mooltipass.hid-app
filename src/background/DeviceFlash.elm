module DeviceFlash where

import List (..)
import Maybe (andThen, Maybe(..))

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
    , firstChild  : ChildNode
    , service    : ByteString
    }

pData (ParentNode data) = data

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

type alias FlashFavorite =
    { parentNode : FlashAddress
    , childNode  : FlashAddress
    }

emptyFav : FlashFavorite
emptyFav = {parentNode = null, childNode = null}

emptyFlashFavorites = [emptyFav,emptyFav,emptyFav,emptyFav,emptyFav
                      ,emptyFav,emptyFav,emptyFav,emptyFav,emptyFav
                      ,emptyFav,emptyFav,emptyFav,emptyFav,emptyFav]

null : FlashAddress
null = (0,0)

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
    let getLogins firstChild =
            foldChildren (\c z -> c.login::z) [] firstChild
    in foldParents
            (\p z -> (p.service, getLogins p.firstChild)::z)
            []
            firstParent

toFavs : List FlashFavorite -> ParentNode -> List Favorite
toFavs ffs firstParent =
    case firstParent of
        (ParentNode firstParentData) ->
            let parent f =
                    if f.parentNode == null then Nothing
                    else foldParents
                        (\p z -> if p.address == f.parentNode
                                 then Just p else z)
                        Nothing
                        firstParent
                child p f =
                    if f.childNode == null then Nothing
                    else foldChildren
                        (\c z -> if c.address == f.childNode
                                 then Just (p.service,c.login) else z)
                        Nothing
                        p.firstChild
            in map (\f -> parent f `andThen` (\p -> child p f)) ffs
        EmptyParentNode -> emptyFavorites
