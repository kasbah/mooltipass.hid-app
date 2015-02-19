module DeviceFlash where

import List (..)
import Maybe (andThen, Maybe(..))
import Maybe

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

queryParent : (ParentNodeData -> Bool) -> (ParentNodeData -> a)
           -> ParentNode -> Maybe a
queryParent fb fp firstParent =
    foldParents
        (\p z -> if z == Nothing && fb p then Just (fp p) else z)
        Nothing
        firstParent

queryChild : (ChildNodeData -> Bool) -> (ChildNodeData -> a)
          -> ChildNode -> Maybe a
queryChild fb fc firstChild =
    foldChildren
        (\c z -> if z == Nothing && fb c then Just (fc c) else z)
        Nothing
        firstChild

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
    let parent fav =
            queryParent
                (\p -> fav.parentNode /= null && p.address == fav.parentNode)
                identity
                firstParent
        child fav p =
            queryChild
                (\c -> fav.childNode /= null && c.address == fav.childNode)
                (\c -> (p.service, c.login))
                p.firstChild
    in map (\f -> parent f `andThen` child f) ffs

fromFavs : List Favorite -> ParentNode -> List OutgoingPacket
fromFavs fs firstParent =
    let parent fav =
            queryParent
                (\p -> fav /= Nothing && p.service == fst (fromJust fav))
                identity
                firstParent
        child fav p =
            queryChild
                (\c -> c.login == snd (fromJust fav))
                (\c -> (p.address, c.address))
                p.firstChild
    in map OutgoingSetFavorite
        <| map2 (,) [1..15]
            <| map (Maybe.withDefault (null, null))
                <| map (\f -> parent f `andThen` child f) fs

