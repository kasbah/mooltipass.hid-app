import DeviceFlash (..)
import Byte (..)
import Util (..)

import Check (..)
import Random (..)
import Text (..)
import Bitwise (..)
import String
import Result
import Debug
import Debug (..)
import List (length)
import Maybe

pairOf : Generator a -> Generator (a,a)
pairOf generator = (,) `map` generator `andMap` generator

tripleOf : Generator a -> Generator (a,a,a)
tripleOf generator = (,,) `map` generator `andMap` generator `andMap` generator

flashAddress : Generator FlashAddress
flashAddress = pairOf notNull

ctr : Generator (Byte,Byte,Byte)
ctr = tripleOf byte

byte : Generator Byte
byte = int 0 255

notNull : Generator Byte
notNull = int 1 255

parentFlags : Generator (Byte, Byte)
parentFlags = map (\(x,b) -> (x,b `and` 0x3F)) (pairOf byte)

childFlags : Generator (Byte, Byte)
childFlags = map (\(x,b) -> (x,b `and` 0x3F `or` 0x40)) (pairOf byte)

byteArray : Int -> Generator ByteArray
byteArray maxLength = list maxLength byte

byteString : Int -> Generator ByteString
byteString maxLength =
    int 0 maxLength `andThen` (\n -> map intsToString (list n notNull))

map : (a -> b) -> Generator a -> Generator b
map f (Generator g) = Generator <| (\(v,seed) -> (f v, seed)) << g

map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 func generatorA generatorB =
    generatorA
        `andThen` \a -> generatorB
        `andThen` \b -> always (func a b)

andThen : Generator a -> (a -> Generator b) -> Generator b
andThen (Generator g) f = Generator <| \seed ->
    let (v,seed')     = g seed
        (Generator h) = f v
    in h seed'

always x = customGenerator <| \seed -> (x,seed)

andMap : Generator (a -> b) -> Generator a -> Generator b
andMap = map2 (<|)

parentNode :
    FlashAddress
    -> (Byte,Byte)
    -> FlashAddress
    -> FlashAddress
    -> List ChildNode
    -> ByteString
    -> ParentNode
parentNode a f n p fc s =
        { address  = a
        , flags    = f
        , next     = n
        , prev     = p
        , children = fc
        , service  = s
        }

childNode :
    FlashAddress
    -> (Byte,Byte)
    -> FlashAddress
    -> FlashAddress
    -> (Byte, Byte, Byte)
    -> ByteString
    -> ByteString
    -> ByteArray
    -> (Byte, Byte)
    -> (Byte, Byte)
    -> ChildNode
childNode a f n p c d l pw dC dU =
        { address      = a
        , flags        = f
        , next         = n
        , prev         = p
        , ctr          = c
        , description  = d
        , login        = l
        , password     = pw
        , dateCreated  = dC
        , dateLastUsed = dU
        }

genChildNode : Generator ChildNode
genChildNode =
    childNode
        `map` flashAddress
        `andMap` childFlags
        `andMap` always nullAddress     -- next child, linked later
        `andMap` always nullAddress     -- prev child, linked later
        `andMap` tripleOf byte   -- ctr
        `andMap` byteString 23   -- description
        `andMap` byteString 62   -- login
        `andMap` byteArray 32    -- password
        `andMap` pairOf byte     -- date last used
        `andMap` pairOf byte     -- date created

genParentNode : Int -> Int -> Generator ParentNode
genParentNode minChildren maxChildren =
    parentNode
    `map` flashAddress
    `andMap` parentFlags
    `andMap` always nullAddress
    `andMap` always nullAddress
    `andMap` (map linkNodes (childNodes minChildren maxChildren))
    `andMap` byteString 32

childNodes : Int -> Int -> Generator (List ChildNode)
childNodes minChildren maxChildren =
    int minChildren maxChildren `andThen` (\n -> list n genChildNode)

flashData : Int -> Int -> Generator (List ParentNode)
flashData maxChildren maxParents =
    map2 (,) (int 1 maxParents) (int 1 maxChildren)
    `andThen` (\(np,maxC) -> map linkNodes (list np (genParentNode 1 maxC)))

tests =
    let writeThenParseParentSucceeds (d,addr) =
            isOk (parse ([],addr,nullAddress) (parentToArray d))
        dataFromParse (Ok ((d::_),_,_)) = d
        writeThenParseParentRetains (d,addr) =
            dataFromParse
                (parse ([],addr,nullAddress) (parentToArray d))
                    == {d | address <- addr}
        writeThenParseParentRetains2 (d,addr) =
            parentToArray
                (dataFromParse
                    (parse ([],addr,nullAddress) (parentToArray d)))
                        == parentToArray d
        writeThenParseChildSucceeds (ps,cd,addr) =
            isOk (parse (ps,addr,nullAddress) (childToArray cd))
        writeThenParseChildRetains (ps,cd,addr) =
            last
            (.children
            (dataFromParse
                (parse (ps,addr,nullAddress) (childToArray cd))))
                    == Just {cd | address <- addr}
        writeThenParseChildRetains2 (d,cd,addr) =
            Maybe.map childToArray (last (.children (dataFromParse (parse (d,addr,nullAddress) (childToArray cd))))) == Just (childToArray cd)
    in simpleCheck
    [ property "- 'null term string length remains the same'"
        (\str -> Result.map String.length (nullTermString (String.length str + 3) ((stringToInts str) ++ [0, 0, 0])) == Ok (String.length str))
        (byteString 255)
    , property "- 'Write parent node is nodeSize bytes'"
        (\d -> length (parentToArray d) == nodeSize)
        (genParentNode 0 0)
    , property "- 'Write child node is nodeSize bytes'"
        (\d -> length (childToArray d) == nodeSize)
        genChildNode
    , property "- 'Write then parse parent'"
        writeThenParseParentSucceeds
        (map2 (,) (genParentNode 0 0) flashAddress)
    , property "- 'Write then parse parent retains'"
        writeThenParseParentRetains
        (map2 (,) (genParentNode 0 0 ) flashAddress)
    , property "- 'Write then parse parent retains 2'"
        writeThenParseParentRetains2
        (map2 (,) (genParentNode 0 0) flashAddress)
    , property "- 'Write then parse child'"
        writeThenParseChildSucceeds
        ((,,) `map` (flashData 1 1) `andMap` genChildNode `andMap` flashAddress)
    , property "- 'Write then parse child retains'"
        writeThenParseChildRetains
        ((,,) `map` (flashData 1 1) `andMap` genChildNode `andMap` flashAddress)
    , property "- 'Write then parse child retains 2'"
        writeThenParseChildRetains2
        ((,,) `map` (flashData 1 1) `andMap` genChildNode `andMap` flashAddress)
    , property "- 'Credential conversion retains'"
        (\p -> fromCreds (toCreds p) == p) (flashData 10 10)
    ]

main = display tests
