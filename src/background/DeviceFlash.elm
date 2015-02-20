module DeviceFlash where

import List (..)
import Maybe (andThen, Maybe(..))
import Maybe
import Bitwise (..)

-- local source
import Byte (..)
import DevicePacket (..)
import CommonState (..)
import Util (..)

type ParentNode = ParentNode ParentNodeData | EmptyParentNode

type alias ParentNodeData =
    { address    : FlashAddress
    , nextParent : ParentNode
    , prevParent : ParentNode
    , firstChild : ChildNode
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

type alias FlashFavorite =
    { parentNode : FlashAddress
    , childNode  : FlashAddress
    }

emptyFav : FlashFavorite
emptyFav = {parentNode = null, childNode = null}

emptyFlashFavorites = [emptyFav,emptyFav,emptyFav,emptyFav,emptyFav
                      ,emptyFav,emptyFav,emptyFav,emptyFav,emptyFav
                      ,emptyFav,emptyFav,emptyFav,emptyFav,emptyFav]


foldrParents : (ParentNodeData -> a -> a) -> a -> ParentNode -> a
foldrParents f z n = case n of
    ParentNode data -> f data (foldrParents f z data.prevParent)
    EmptyParentNode -> z

foldrChildren : (ChildNodeData -> a -> a) -> a -> ChildNode -> a
foldrChildren f z n = case n of
    ChildNode data -> f data (foldrChildren f z data.prevChild)
    EmptyChildNode -> z

firstParent : ParentNode -> ParentNode
firstParent parent = foldrParents (\d _ -> ParentNode d) parent parent

firstChild : ChildNode -> ChildNode
firstChild child = foldrChildren (\d _ -> ChildNode d) child child

lastChild : ParentNodeData -> ChildNode
lastChild pdata = foldlChildren (\d _ -> ChildNode d) pdata.firstChild pdata.firstChild

parentAddress : ParentNode -> FlashAddress
parentAddress p = case p of
    (ParentNode data) -> data.address
    _                 -> null

parentAddress' : ChildNode -> ParentNode -> Maybe FlashAddress
parentAddress' c p = queryParent
    (\d -> d.firstChild == (firstChild c))
    .address
    (firstParent p)

null : FlashAddress
null = (0,0)

foldlChildren : (ChildNodeData -> a -> a) -> a -> ChildNode -> a
foldlChildren f z n = case n of
    ChildNode data -> f data (foldlChildren f z data.nextChild)
    EmptyChildNode -> z

foldlParents : (ParentNodeData -> a -> a) -> a -> ParentNode -> a
foldlParents f z n = case n of
    ParentNode data -> f data (foldlParents f z data.nextParent)
    EmptyParentNode -> z

queryParent : (ParentNodeData -> Bool) -> (ParentNodeData -> a)
           -> ParentNode -> Maybe a
queryParent fb fp firstP =
    foldlParents
        (\p z -> if z == Nothing && fb p then Just (fp p) else z)
        Nothing
        firstP

queryChild : (ChildNodeData -> Bool) -> (ChildNodeData -> a)
          -> ChildNode -> Maybe a
queryChild fb fc firstC =
    foldlChildren
        (\c z -> if z == Nothing && fb c then Just (fc c) else z)
        Nothing
        firstC

toCreds : ParentNode -> List (String, List String)
toCreds firstP =
    let getLogins firstC =
            foldlChildren (\c z -> c.login::z) [] firstC
    in foldlParents
            (\p z -> (p.service, getLogins p.firstChild)::z)
            []
            firstP

toFavs : List FlashFavorite -> ParentNode -> List Favorite
toFavs ffs firstP =
    let parent fav =
            queryParent
                (\p -> fav.parentNode /= null && p.address == fav.parentNode)
                identity
                firstP
        child fav p =
            queryChild
                (\c -> fav.childNode /= null && c.address == fav.childNode)
                (\c -> (p.service, c.login))
                p.firstChild
    in map (\f -> parent f `andThen` child f) ffs

fromFavs : List Favorite -> ParentNode -> List OutgoingPacket
fromFavs fs firstP =
    let parent fav =
            queryParent
                (\p -> fav /= Nothing && p.service == fst (fromJust fav))
                identity
                firstP
        child fav p =
            queryChild
                (\c -> c.login == snd (fromJust fav))
                (\c -> (p.address, c.address))
                p.firstChild
    in map OutgoingSetFavorite
        <| map2 (,) [1..15]
            <| map (Maybe.withDefault (null, null))
                <| map (\f -> parent f `andThen` child f) fs


addParentNode : ParentNode -> FlashAddress -> ByteArray
              -> Maybe (ParentNode, FlashAddress)
addParentNode p addr bs = case bs of
    (flags1::flags2::prevP1::prevP2::nextP1::nextP2::firstC1::firstC2::service) ->
        Just (pNode addr (intsToString service) p, (firstC1,firstC2))
    _ -> Nothing

pNode : FlashAddress -> String -> ParentNode -> ParentNode
pNode addr str p = ParentNode
                { address    = addr
                , nextParent = EmptyParentNode
                , prevParent = p
                , firstChild = EmptyChildNode
                , service    = str
                }

parse : FlashAddress -> ByteArray -> Maybe (ParentNode, FlashAddress)
parse addr bs =
    let parentOrChild = case bs of
            (_::flags2::_) -> (flags2 `and` 0xC000) `shiftRight` 12
            _ -> (-1)
    in case parentOrChild of
        --0 -> addParentNode addr bs
        --1 -> Maybe.map addChildNode addr bs
        _ -> Nothing
