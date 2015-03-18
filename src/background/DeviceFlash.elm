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

nodeSize = 132

type alias ParentNode =
    { address    : FlashAddress
    , flags      : (Byte,Byte)
    , nextParent : FlashAddress
    , prevParent : FlashAddress
    , firstChild : List ChildNode
    , service    : ByteString
    }

type alias ChildNode =
    { address      : FlashAddress
    , flags        : (Byte,Byte)
    , nextChild    : FlashAddress
    , prevChild    : FlashAddress
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

emptyFlashFavorites = map (\_ -> emptyFav) emptyFavorites

toCreds : List ParentNode -> List Service
toCreds ps =
    let getLogins firstC =
            reverse <| foldl (\c z -> removeChildren c::z) [] firstC
        removeChildren c = let c' = {c - nextChild} in {c' - prevChild}
        removeNodes p = let p'  = {p - prevParent}
                            p'' = {p' - nextParent}
                        in {p'' - firstChild}
    in reverse <| foldl
            (\p z -> (removeNodes p, getLogins p.firstChild)::z)
            []
            ps

toFavs : List FlashFavorite -> List ParentNode -> List Favorite
toFavs ffs ps =
    let parent fav =
            maybeHead
            <| filter
                (\p -> fav.parentNode /= null && p.address == fav.parentNode)
                ps
        child fav p =
            Maybe.map (\c -> (p.address, c.address))
                <| maybeHead
                    <| filter
                        (\c -> fav.childNode /= null && c.address == fav.childNode)
                        p.firstChild
    in reverse <| map (\f -> parent f `andThen` child f) ffs


favsToPackets : List Favorite -> List OutgoingPacket
favsToPackets fs =
    map OutgoingSetFavorite
        <| map2 (,) [0..maxFavs]
            <| map (Maybe.withDefault (null, null)) fs


credsToPackets : List Service -> List ParentNode -> List OutgoingPacket
credsToPackets creds ps = Debug.crash ""


headAddress : List { a | address : FlashAddress } -> FlashAddress
headAddress nodes = Maybe.withDefault null (Maybe.map (.address) (maybeHead nodes))

fromLogins : List Login -> List ChildNode
fromLogins ls =
    let newChild l =
            { address      = l.address
            , flags        = l.flags
            , nextChild    = null
            , prevChild    = null
            , ctr          = l.ctr
            , description  = l.description
            , login        = l.login
            , password     = l.password
            , dateCreated  = l.dateCreated
            , dateLastUsed = l.dateLastUsed
            }
    in linkKids <| map newChild ls

linkParents : List ParentNode -> List ParentNode
linkParents ps =
    let linkPrev p z = {p | prevParent <- headAddress z}::z
        linkNext p z = {p | nextParent <- headAddress z}::z
    in foldr linkNext [] <| foldl linkPrev [] ps

linkKids : List ChildNode -> List ChildNode
linkKids ps =
    let linkPrev p z = {p | prevChild <- headAddress z}::z
        linkNext p z = {p | nextChild <- headAddress z}::z
    in foldr linkNext [] <| foldl linkPrev [] ps

fromCreds : List Service -> List ParentNode
fromCreds creds =
    let newParent (sName, logins)=
            { address    = sName.address
            , flags      = sName.flags
            , nextParent = null
            , prevParent = null
            , firstChild = fromLogins logins
            , service    = sName.service
            }
    in linkParents <| map newParent creds

credsToDelete : List Service -> List ParentNode -> List OutgoingPacket
credsToDelete creds ps =
    foldl
        (\pNode z ->
            if not (any (\(sName,_) -> sName.address == pNode.address) creds)
            then z ++ deleteNodePackets pNode.address else z)
        []
        ps

deleteNodePackets : FlashAddress -> List OutgoingPacket
deleteNodePackets addr =
    [ OutgoingWriteFlashNode addr 0 (repeat 59 0xFF)
    , OutgoingWriteFlashNode addr 1 []
    , OutgoingWriteFlashNode addr 2 []
    ]

parseParentNode : FlashAddress -> ByteArray
                -> Maybe (ParentNode, FlashAddress, FlashAddress)
parseParentNode addr bs =
    case bs of
        (flags1::flags2::prevP1::prevP2::nextP1::nextP2::firstC1::firstC2::service) ->
            case nullTermString 58 service of
            Ok str ->
                let newP =
                        ( { address    = addr
                          , flags      = (flags1,flags2)
                          , nextParent = (nextP1,nextP2)
                          , prevParent = (prevP1,prevP2)
                          , firstChild = []
                          , service    = str
                          }
                        , (firstC1,firstC2), (nextP1,nextP2))
                in Just newP
            Err _ -> Nothing
        _ -> Nothing

parseChildNode : ParentNode -> FlashAddress -> FlashAddress -> ByteArray
               -> Result String (ParentNode, FlashAddress, FlashAddress)
parseChildNode d addr nParentAddr bs =
        let cNodeAndNextAddr = case bs of
                (flags1::flags2::prevC1::prevC2::nextC1::nextC2::data) ->
                    case nullTermString 24 data of
                        Ok descr -> case drop 24 data of
                            (dateC1::dateC2::dateU1::dateU2::ctr1::ctr2::ctr3::data') ->
                                case nullTermString 63 data' of
                                    Ok login -> case toByteArray 32 (drop 63 data') of
                                        Ok pw ->
                                            Ok (
                                                { address      = addr
                                                , flags        = (flags1,flags2)
                                                , nextChild    = (nextC1, nextC2)
                                                , prevChild    = (prevC1, prevC2)
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
                ({d | firstChild <- d.firstChild ++ [cNode]}
                , if nAddr == null then nParentAddr else nAddr
                , nParentAddr)
        in Result.map pNodeAndNextAddr cNodeAndNextAddr

parse : (ParentNode, FlashAddress, FlashAddress) -> ByteArray
      -> Result String (ParentNode, FlashAddress, FlashAddress)
parse (p,addr,nParentAddr) bs =
    let parentOrChild = case bs of
            (_::flags2::_) -> (flags2 `and` 0xC0) `shiftRight` 6
            _ -> (-1)
    in case parentOrChild of
        0 -> fromMaybe "parse parent failed" <| parseParentNode addr bs
        1 -> parseChildNode p addr nParentAddr bs
        _ -> Err <| "Invalid flags: " ++ (toString parentOrChild)

parentToPackets : ParentNode -> List OutgoingPacket
parentToPackets d =
    let ba = parentToArray d
    in [ OutgoingWriteFlashNode d.address 0 (take 59 ba)
       , OutgoingWriteFlashNode d.address 1 (take 59 (drop 59 ba))
       , OutgoingWriteFlashNode d.address 2 (take 59 (drop 59 (drop 59 ba)))
       ]

childToPackets : ChildNode -> List OutgoingPacket
childToPackets d =
    let ba = childToArray d
    in [ OutgoingWriteFlashNode d.address 0 (take 59 ba)
       , OutgoingWriteFlashNode d.address 1 (take 59 (drop 59 ba))
       , OutgoingWriteFlashNode d.address 2 (take 59 (drop 59 (drop 59 ba)))
       ]

parentToArray : ParentNode -> ByteArray
parentToArray d =
    let data =
        pairToList d.flags
        ++ pairToList d.prevParent
        ++ pairToList d.nextParent
        ++ pairToList (headAddress d.firstChild)
        ++ stringToInts d.service
    in data ++ repeat (nodeSize - length data) 0

childToArray : ChildNode-> ByteArray
childToArray d =
    let descr = stringToInts d.description
        login = stringToInts d.login
    in pairToList d.flags
    ++ pairToList d.prevChild
    ++ pairToList d.nextChild
    ++ descr ++ (repeat (24 - length descr) 0)
    ++ pairToList d.dateCreated
    ++ pairToList d.dateLastUsed
    ++ (\(x,y,z) -> [x,y,z]) d.ctr
    ++ login ++ (repeat (63 - length login) 0)
    ++ d.password
