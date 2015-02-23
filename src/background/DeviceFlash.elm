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
    , flags      : (Byte,Byte)
    , nextParent : ParentNode
    , prevParent : ParentNode
    , firstChild : ChildNode
    , service    : ByteString
    }

parentNode :
    FlashAddress
    -> (Byte,Byte)
    -> ParentNode
    -> ParentNode
    -> ChildNode
    -> ByteString
    -> ParentNode
parentNode a f n p fc s =
    ParentNode
        { address    = a
        , flags      = f
        , nextParent = n
        , prevParent = p
        , firstChild = fc
        , service    = s
        }

type ChildNode = ChildNode ChildNodeData | EmptyChildNode

type alias ChildNodeData =
    { address      : FlashAddress
    , flags        : (Byte,Byte)
    , nextChild    : ChildNode
    , prevChild    : ChildNode
    , ctr          : (Byte, Byte, Byte)
    , description  : ByteString
    , login        : ByteString
    , password     : ByteArray
    , dateCreated  : (Byte, Byte)
    , dateLastUsed : (Byte, Byte)
    }

childNode :
    FlashAddress
    -> (Byte,Byte)
    -> ChildNode
    -> ChildNode
    -> (Byte, Byte, Byte)
    -> ByteString
    -> ByteString
    -> ByteArray
    -> (Byte, Byte)
    -> (Byte, Byte)
    -> ChildNode
childNode a f n p c d l pw dC dU =
    ChildNode
        { address      = a
        , flags        = f
        , nextChild    = n
        , prevChild    = p
        , ctr          = c
        , description  = d
        , login        = l
        , password     = pw
        , dateCreated  = dC
        , dateLastUsed = dU
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
firstParent parent = foldrParents (\d _ -> ParentNode d) EmptyParentNode parent

firstChild : ChildNode -> ChildNode
firstChild child = foldrChildren (\d _ -> ChildNode d) EmptyChildNode child

linkNextParentsReturnFirst : ParentNode -> ParentNode
linkNextParentsReturnFirst parent =
    foldrParents (\d z -> ParentNode {d | nextParent <- z}) EmptyParentNode (lastParent parent)

linkPrevParentsReturnLast : ParentNode -> ParentNode
linkPrevParentsReturnLast parent =
    foldlParents (\d z -> ParentNode {d | prevParent <- z}) EmptyParentNode (firstParent parent)

linkNextChildrenReturnFirst : ChildNode -> ChildNode
linkNextChildrenReturnFirst child =
    foldrChildren (\d z -> ChildNode {d | nextChild <- z}) EmptyChildNode (lastChild child)

linkPrevChildrenReturnLast : ChildNode -> ChildNode
linkPrevChildrenReturnLast child =
    foldlChildren (\d z -> ChildNode {d | prevChild <- z}) EmptyChildNode (firstChild child)

lastChild : ChildNode -> ChildNode
lastChild child = foldlChildren (\d _ -> ChildNode d) EmptyChildNode child

lastParent : ParentNode -> ParentNode
lastParent parent = foldlParents (\d _ -> ParentNode d) EmptyParentNode parent

mapParents : (ParentNode -> ParentNode) -> ParentNode -> ParentNode
mapParents fn p =
    linkNextParentsReturnFirst
        <| foldlParents
            (\d z -> ParentNode {d | prevParent <- fn z})
            EmptyParentNode
            p

pAddress : ParentNode -> FlashAddress
pAddress p = case p of
    (ParentNode data) -> data.address
    _                 -> null

cAddress : ChildNode -> FlashAddress
cAddress p = case p of
    (ChildNode data) -> data.address
    _                 -> null

parentAddress' : ChildNode -> ParentNode -> Maybe FlashAddress
parentAddress' c p = queryParents
    (\d -> d.firstChild == (firstChild c))
    .address
    p

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

queryParents : (ParentNodeData -> Bool) -> (ParentNodeData -> a)
           -> ParentNode -> Maybe a
queryParents fb fp p =
    foldlParents
        (\p z -> if z == Nothing && fb p then Just (fp p) else z)
        Nothing
        (firstParent p)

queryChildren : (ChildNodeData -> Bool) -> (ChildNodeData -> a)
          -> ChildNode -> Maybe a
queryChildren fb fc c =
    foldlChildren
        (\c z -> if z == Nothing && fb c then Just (fc c) else z)
        Nothing
        (firstChild c)

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
            queryParents
                (\p -> fav.parentNode /= null && p.address == fav.parentNode)
                identity
                firstP
        child fav p =
            queryChildren
                (\c -> fav.childNode /= null && c.address == fav.childNode)
                (\c -> (p.service, c.login))
                p.firstChild
    in map (\f -> parent f `andThen` child f) ffs

fromFavs : List Favorite -> ParentNode -> List OutgoingPacket
fromFavs fs firstP =
    let parent fav =
            queryParents
                (\p -> fav /= Nothing && p.service == fst (fromJust fav))
                identity
                firstP
        child fav p =
            queryChildren
                (\c -> c.login == snd (fromJust fav))
                (\c -> (p.address, c.address))
                p.firstChild
    in map OutgoingSetFavorite
        <| map2 (,) [1..15]
            <| map (Maybe.withDefault (null, null))
                <| map (\f -> parent f `andThen` child f) fs


parseParentNode : ParentNode -> FlashAddress -> ByteArray
              -> Maybe (ParentNode, FlashAddress)
parseParentNode p addr bs =
    case bs of
        (flags1::flags2::prevP1::prevP2::nextP1::nextP2::firstC1::firstC2::service) ->
            let newP =
                    (ParentNode
                        { address    = addr
                        , flags      = (flags1,flags2)
                        , nextParent = EmptyParentNode
                        , prevParent = p
                        , firstChild = EmptyChildNode
                        , service    = intsToString service
                        }
                    , (firstC1,firstC2))
            in Just newP
        _ -> Nothing


parseChildNode : ParentNode -> FlashAddress -> ByteArray
            -> Maybe (ParentNode, FlashAddress)
parseChildNode p addr bs = case p of
    ParentNode d ->
        let cNodeAndNextAddr = case bs of
                (flags1::flags2::nextC1::nextC2::ctr1::ctr2::ctr3::data) ->
                    case toByteString 24 data of
                        Ok descr -> case toByteString 63 (drop 24 data) of
                            Ok login -> case toByteArray 32 (drop 63 (drop 24 data)) of
                                Ok pw -> case drop 32 (drop 63 (drop 24 data)) of
                                    (dateC1::dateC2::dateU1::[dateU2]) ->
                                        Just
                                            (ChildNode
                                                { address      = addr
                                                , flags        = (flags1,flags2)
                                                , nextChild    = EmptyChildNode
                                                , prevChild    = lastChild d.firstChild
                                                , ctr          = (ctr1,ctr2,ctr3)
                                                , description  = descr
                                                , login        = login
                                                , password     = pw
                                                , dateCreated  = (dateC1,dateC2)
                                                , dateLastUsed = (dateU1,dateU2)
                                                }
                                            , (nextC1, nextC2))
                                    _ -> Nothing
                                Err _ -> Nothing
                            Err _ -> Nothing
                        Err _ -> Nothing
                _ -> Nothing
            pNodeAndNextAddr (cN, nAddr) = (ParentNode {d | firstChild <- linkNextChildrenReturnFirst cN}, nAddr)
        in Maybe.map pNodeAndNextAddr cNodeAndNextAddr
    EmptyParentNode -> Nothing

parse : ParentNode -> FlashAddress -> ByteArray -> Maybe (ParentNode, FlashAddress)
parse p addr bs =
    let parentOrChild = case bs of
            (_::flags2::_) -> (flags2 `and` 0xC0) `shiftRight` 6
            _ -> (-1)
    in case parentOrChild of
        0 -> parseParentNode p addr bs
        1 -> parseChildNode p addr bs
        _ -> Nothing

pairToList : (a,a) -> List a
pairToList (x,y) = [x,y]

parentToArray : ParentNodeData -> ByteArray
parentToArray d =
    pairToList d.flags
    ++ pairToList (pAddress d.prevParent)
    ++ pairToList (pAddress d.nextParent)
    ++ pairToList (cAddress d.firstChild)
    ++ stringToInts d.service

childToArray : ChildNodeData -> ByteArray
childToArray d =
    pairToList d.flags
    ++ pairToList (cAddress d.prevChild)
    ++ pairToList (cAddress d.nextChild)
    ++ (\(x,y,z) -> [x,y,z]) d.ctr
    ++ stringToInts d.description
    ++ stringToInts d.login
    ++ d.password
    ++ pairToList d.dateCreated
    ++ pairToList d.dateLastUsed
