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

type alias Node a =
    { a | address : FlashAddress
    , flags   : (Byte,Byte)
    , next    : FlashAddress
    , prev    : FlashAddress
    }

type alias ParentNode =
    { address  : FlashAddress
    , flags    : (Byte,Byte)
    , next     : FlashAddress
    , prev     : FlashAddress
    , children : List ChildNode
    , service  : ByteString
    }

type alias ChildNode =
    { address      : FlashAddress
    , flags        : (Byte,Byte)
    , next         : FlashAddress
    , prev         : FlashAddress
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

emptyFlashFavorites = repeat maxFavs emptyFavorites

toCreds : List ParentNode -> List Service
toCreds ps =
    let getLogins firstC   = map removeAddresses firstC
        removeAddresses c  = let c' = {c - next} in {c' - prev}
        removeAddresses' p = let p'  = removeAddresses p
                             in {p' - children}
    in filter (\(s,ls) -> not (isEmpty ls)) <| reverse <| map (\p -> (removeAddresses' p, getLogins p.children)) ps

fromCreds : List Service -> List ParentNode
fromCreds creds =
    let newParent (sName, logins)=
            { address  = sName.address
            , flags    = sName.flags
            , next     = null
            , prev     = null
            , children = fromLogins logins
            , service  = sName.service
            }
    in linkNodes <| map newParent <| creds

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
                        p.children
    in reverse <| map (\f -> parent f `andThen` child f) ffs


favsToPackets : List Favorite -> List OutgoingPacket
favsToPackets fs =
    map OutgoingSetFavorite
        <| map2 (,) [0..maxFavs]
            <| map (Maybe.withDefault (null, null)) fs

credsToPackets : List Service -> List ParentNode -> List OutgoingPacket
credsToPackets creds ps =
    let newPNodes  = fromCreds creds
        delPackets = credsToDelete creds ps
        newCs cs   = concat (map childToPackets cs)
        newPackets = concat
            <| map (\p -> parentToPackets p ++ newCs p.children) newPNodes
        newStartingP =
            [OutgoingSetStartingParent
                <| Maybe.withDefault null
                    <| Maybe.map (.address)
                        <| maybeHead newPNodes]
    in newStartingP ++ newPackets ++ delPackets

headAddress : List (Node a) -> FlashAddress
headAddress nodes = Maybe.withDefault null (Maybe.map (.address) (maybeHead nodes))

fromLogins : List Login -> List ChildNode
fromLogins ls =
    let newChild l =
            { address      = l.address
            , flags        = l.flags
            , next         = null
            , prev         = null
            , ctr          = l.ctr
            , description  = l.description
            , login        = l.login
            , password     = l.password
            , dateCreated  = l.dateCreated
            , dateLastUsed = l.dateLastUsed
            }
    in linkNodes <| map newChild ls

linkNodes : List (Node a) -> List (Node a)
linkNodes ns =
    let linkPrev p z = {p | prev <- headAddress z}::z
        linkNext p z = {p | next <- headAddress z}::z
    in reverse <| foldl linkPrev [] <| foldr linkNext [] ns

credsToDelete : List Service -> List ParentNode -> List OutgoingPacket
credsToDelete creds ps =
    let findDeletedKids p (sName, ls) z =
            if sName.address == p.address
            then z ++ foldl (findDeletedKids' ls) [] p.children
            else z
        findDeletedKids' logins c z =
            if not (any (\l -> l.address == c.address) logins)
            then z ++ deleteNodePackets c.address
            else z
        findDeleted pNode z =
            if not (any (\(sName,_) -> sName.address == pNode.address) creds)
            then z ++ deleteNodePackets pNode.address ++ concat (map (deleteNodePackets << .address) pNode.children)
            else z ++ foldl (findDeletedKids pNode) [] creds
    in foldl findDeleted [] ps

deleteNodePackets : FlashAddress -> List OutgoingPacket
deleteNodePackets addr =
    [ OutgoingWriteFlashNode addr 0 [0xFF, 0xFF]
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
                        ( { address  = addr
                          , flags    = (flags1,flags2)
                          , next     = (nextP1,nextP2)
                          , prev     = (prevP1,prevP2)
                          , children = []
                          , service  = str
                          }
                        , if (firstC1,firstC2) /= null
                          then (firstC1,firstC2)
                          else (nextP1,nextP2)
                        , (nextP1,nextP2))
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
                                                , next         = (nextC1, nextC2)
                                                , prev         = (prevC1, prevC2)
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
                ({d | children <- d.children ++ [cNode]}
                , if nAddr == null then nParentAddr else nAddr
                , nParentAddr)
        in Result.map pNodeAndNextAddr cNodeAndNextAddr

parse : (List ParentNode, FlashAddress, FlashAddress) -> ByteArray
      -> Result String (List ParentNode, FlashAddress, FlashAddress)
parse (ps,addr,nParentAddr) bs =
    let parentOrChild = case bs of
            (_::flags2::_) -> (flags2 `and` 0xC0) `shiftRight` 6
            _ -> (-1)
    in case parentOrChild of
        0 -> Result.map (\(p,a1,a2) -> (p::ps,a1,a2)) <| fromMaybe "parse parent failed" <| parseParentNode addr bs
        1 -> case ps of
            (p::ps') -> Result.map (\(p',a1,a2) -> (p'::ps',a1,a2)) <| parseChildNode p addr nParentAddr bs
            _ -> Err <| "No parent nodes when parsing child node"
        _ -> Err <| "Invalid flags: " ++ (toString parentOrChild)

parentToPackets : ParentNode -> List OutgoingPacket
parentToPackets d = toPackets d.address (parentToArray d)

childToPackets : ChildNode -> List OutgoingPacket
childToPackets d = toPackets d.address (childToArray d)

toPackets : FlashAddress -> ByteArray -> List OutgoingPacket
toPackets addr ba =
    [ OutgoingWriteFlashNode addr 0 (take 8 ba)
    , OutgoingWriteFlashNode addr 1 [] --(take 59 (drop 59 ba))
    , OutgoingWriteFlashNode addr 2 [] --(take 59 (drop 59 (drop 59 ba)))
    ]

parentToArray : ParentNode -> ByteArray
parentToArray d =
    let data =
        pairToList d.flags
        ++ pairToList d.prev
        ++ pairToList d.next
        ++ pairToList (headAddress d.children)
        ++ stringToInts d.service
    in data ++ repeat (nodeSize - length data) 0

childToArray : ChildNode -> ByteArray
childToArray d =
    let descr = stringToInts d.description
        login = stringToInts d.login
    in pairToList d.flags
    ++ pairToList d.prev
    ++ pairToList d.next
    ++ descr ++ (repeat (24 - length descr) 0)
    ++ pairToList d.dateCreated
    ++ pairToList d.dateLastUsed
    ++ (\(x,y,z) -> [x,y,z]) d.ctr
    ++ login ++ (repeat (63 - length login) 0)
    ++ d.password
