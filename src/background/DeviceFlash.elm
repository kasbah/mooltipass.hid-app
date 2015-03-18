module DeviceFlash where

import List (..)
import Maybe (andThen, Maybe(..))
import Maybe
import Result (fromMaybe, Result(..))
import Result
import Bitwise (..)
import String
import Debug

-- local source
import Byte (..)
import DevicePacket (..)
import CommonState (..)
import Util (..)

type ParentNode = ParentNode ParentNodeData | EmptyParentNode

nodeSize = 132

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

emptyFlashFavorites = map (\_ -> emptyFav) emptyFavorites

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
    foldrParents
        (\d z -> ParentNode {d | nextParent <- z})
        EmptyParentNode
        (lastParent parent)

linkPrevParentsReturnLast : ParentNode -> ParentNode
linkPrevParentsReturnLast parent =
    foldlParents
        (\d z -> ParentNode {d | prevParent <- z})
        EmptyParentNode
        (firstParent parent)

linkNextChildrenReturnFirst : ChildNode -> ChildNode
linkNextChildrenReturnFirst child =
    foldrChildren
        (\d z -> ChildNode {d | nextChild <- z})
        EmptyChildNode
        (lastChild child)

linkPrevChildrenReturnLast : ChildNode -> ChildNode
linkPrevChildrenReturnLast child =
    foldlChildren
        (\d z -> ChildNode {d | prevChild <- z})
        EmptyChildNode
        (firstChild child)

lastChild : ChildNode -> ChildNode
lastChild child = foldlChildren (\d _ -> ChildNode d) EmptyChildNode child

lastParent : ParentNode -> ParentNode
lastParent parent = foldlParents (\d _ -> ParentNode d) EmptyParentNode parent

mapParents : (ParentNodeData -> a) -> ParentNode -> List a
mapParents fn parent =
        foldrParents
            (\d z -> fn d::z)
            []
            (lastParent parent)

pAddress : ParentNode -> FlashAddress
pAddress p = case p of
    (ParentNode data) -> data.address
    _                 -> null

cAddress : ChildNode -> FlashAddress
cAddress p = case p of
    (ChildNode data) -> data.address
    _                -> null

parentAddress' : ChildNode -> ParentNode -> Maybe FlashAddress
parentAddress' c p = queryParents
    (\d -> d.firstChild == (firstChild c))
    .address
    p

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

toCreds : ParentNode -> List Service
toCreds firstP =
    let getLogins firstC =
            reverse <| foldlChildren (\c z -> removeChildren c::z) [] firstC
        removeChildren c = let c' = {c - nextChild} in {c' - prevChild}
        removeNodes p = let p'  = {p - prevParent}
                            p'' = {p' - nextParent}
                        in {p'' - firstChild}
    in reverse <| foldlParents
            (\p z -> (removeNodes p, getLogins p.firstChild)::z)
            []
            firstP

toFavs : List FlashFavorite -> ParentNode -> List Favorite
toFavs ffs firstP =
    let parent fav =
            queryParents
                (\p -> fav.parentNode /= null && p.address == fav.parentNode)
                identity
                firstP
        child fav' p =
            queryChildren
                (\c -> fav'.childNode /= null && c.address == fav'.childNode)
                (\c -> (p.address, c.address))
                p.firstChild
    in reverse <| map (\f -> parent f `andThen` child f) ffs

favsToPackets : List Favorite -> ParentNode -> List OutgoingPacket
favsToPackets fs firstP =
    let parent fav =
            queryParents
                (\p -> fav /= Nothing && p.address == fst (fromJust fav))
                identity
                firstP
        child fav p =
            queryChildren
                (\c -> c.address == snd (fromJust fav))
                (\c -> (p.address, c.address))
                p.firstChild
    in map OutgoingSetFavorite
        <| map2 (,) [0..maxFavs]
            <| map (Maybe.withDefault (null, null))
                <| map (\f -> parent f `andThen` child f) fs


credsToPackets : List Service -> ParentNode -> List OutgoingPacket
credsToPackets creds firstP = Debug.crash ""

fromCreds : List Service -> ParentNode
fromCreds creds =
    let newChild l z =
            ChildNode
                { address      = l.address
                , flags        = l.flags
                , nextChild    = z
                , prevChild    = EmptyChildNode
                , ctr          = l.ctr
                , description  = l.description
                , login        = l.login
                , password     = l.password
                , dateCreated  = l.dateCreated
                , dateLastUsed = l.dateLastUsed
                }
        newParent (sName, logins) z =
            ParentNode
                { address    = sName.address
                , flags      = sName.flags
                , nextParent = z
                , prevParent = EmptyParentNode
                , firstChild = linkPrevChildrenReturnLast
                    <| foldr newChild EmptyChildNode (reverse logins)
                , service    = sName.service
                }
    in linkPrevParentsReturnLast (foldr newParent EmptyParentNode (reverse creds))

credsToDelete : List Service -> ParentNode -> List OutgoingPacket
credsToDelete creds firstP =
    foldlParents
        (\pNode z ->
            if not (any (\(sName,_) -> sName.address == pNode.address) creds)
            then z ++ deleteNodePackets pNode.address else z)
        []
        firstP

parseParentNode : ParentNode -> FlashAddress -> ByteArray
                -> Maybe (ParentNode, FlashAddress, FlashAddress)
parseParentNode p addr bs =
    case bs of
        (flags1::flags2::prevP1::prevP2::nextP1::nextP2::firstC1::firstC2::service) ->
            case nullTermString 58 service of
            Ok str ->
                let newP =
                        (ParentNode
                            { address    = addr
                            , flags      = (flags1,flags2)
                            , nextParent = EmptyParentNode
                            , prevParent = p
                            , firstChild = EmptyChildNode
                            , service    = str
                            }
                        , (firstC1,firstC2), (nextP1,nextP2))
                in Just newP
            Err _ -> Nothing
        _ -> Nothing

parseChildNode : ParentNode -> FlashAddress -> FlashAddress -> ByteArray
               -> Result String (ParentNode, FlashAddress, FlashAddress)
parseChildNode p addr nParentAddr bs = case p of
    ParentNode d ->
        let cNodeAndNextAddr = case bs of
                (flags1::flags2::prevC1::prevC2::nextC1::nextC2::data) ->
                    case nullTermString 24 data of
                        Ok descr -> case drop 24 data of
                            (dateC1::dateC2::dateU1::dateU2::ctr1::ctr2::ctr3::data') ->
                                case nullTermString 63 data' of
                                    Ok login -> case toByteArray 32 (drop 63 data') of
                                        Ok pw ->
                                            Ok (ChildNode
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
                                        Err s ->
                                            Err <| "Converting password, "
                                                ++ s ++ toString bs
                                    Err s -> Err <| "Converting login, " ++ s
                            _ -> Err "Converting dates and ctr"
                        Err s -> Err <| "Converting description, " ++ s
                _ -> Err "Not enough data"
            pNodeAndNextAddr (cNode, nAddr) =
                (ParentNode {d | firstChild <- linkNextChildrenReturnFirst cNode}
                , if nAddr == null then nParentAddr else nAddr
                , nParentAddr)
        in Result.map pNodeAndNextAddr cNodeAndNextAddr
    EmptyParentNode -> Err "Empty parent node"

parse : (ParentNode, FlashAddress, FlashAddress) -> ByteArray
      -> Result String (ParentNode, FlashAddress, FlashAddress)
parse (p,addr,nParentAddr) bs =
    let parentOrChild = case bs of
            (_::flags2::_) -> (flags2 `and` 0xC0) `shiftRight` 6
            _ -> (-1)
    in case parentOrChild of
        0 -> fromMaybe "parse parent failed" <| parseParentNode p addr bs
        1 -> parseChildNode p addr nParentAddr bs
        _ -> Err <| "Invalid flags: " ++ (toString parentOrChild)

parentToPackets : ParentNodeData -> List OutgoingPacket
parentToPackets d =
    let ba = parentToArray d
    in [ OutgoingWriteFlashNode d.address 0 (take 59 ba)
       , OutgoingWriteFlashNode d.address 1 (take 59 (drop 59 ba))
       , OutgoingWriteFlashNode d.address 2 (take 59 (drop 59 (drop 59 ba)))
       ]

deleteNodePackets : FlashAddress -> List OutgoingPacket
deleteNodePackets addr =
    [ OutgoingWriteFlashNode addr 0 (repeat 59 0xFF)
    , OutgoingWriteFlashNode addr 1 []
    , OutgoingWriteFlashNode addr 2 []
    ]

childToPackets : ChildNodeData -> List OutgoingPacket
childToPackets d =
    let ba = childToArray d
    in [ OutgoingWriteFlashNode d.address 0 (take 59 ba)
       , OutgoingWriteFlashNode d.address 1 (take 59 (drop 59 ba))
       , OutgoingWriteFlashNode d.address 2 (take 59 (drop 59 (drop 59 ba)))
       ]

parentToArray : ParentNodeData -> ByteArray
parentToArray d =
    let data =
        pairToList d.flags
        ++ pairToList (pAddress d.prevParent)
        ++ pairToList (pAddress d.nextParent)
        ++ pairToList (cAddress d.firstChild)
        ++ stringToInts d.service
    in data ++ repeat (nodeSize - length data) 0

childToArray : ChildNodeData -> ByteArray
childToArray d =
    let descr = stringToInts d.description
        login = stringToInts d.login
    in pairToList d.flags
    ++ pairToList (cAddress d.prevChild)
    ++ pairToList (cAddress d.nextChild)
    ++ descr ++ (repeat (24 - length descr) 0)
    ++ pairToList d.dateCreated
    ++ pairToList d.dateLastUsed
    ++ (\(x,y,z) -> [x,y,z]) d.ctr
    ++ login ++ (repeat (63 - length login) 0)
    ++ d.password
