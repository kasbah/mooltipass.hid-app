import Check (..)
import Random (..)
import DeviceFlash (..)
import Byte (..)
import Util (..)
import Debug (..)
import List (length)
import String
import Result

import Text (..)
import Bitwise (..)

pairOf : Generator a -> Generator (a,a)
pairOf generator =
        --generator `andThen` (\x -> map (\y -> (x,y)) generator)
        (,) `map` generator `andMap` generator
        --generator `andThen` ((,) `map` generator)

---tripleOf : Generator a -> Generator (a,a,a)
---tripleOf generator =
---    (\x y z -> (x,y,z)) `map` (generator `andThen` (generator `andThen` generator))
tripleOf : Generator a -> Generator (a,a,a)
tripleOf generator = (,,) `map` generator `andMap` generator `andMap` generator

tripleOf' : Generator a -> Generator (a,a,a)
tripleOf' generator =
    customGenerator <| \seed ->
        let (one, seed' )    = generate generator seed
            (two, seed'')    = generate generator seed'
            (three, seed''') = generate generator seed''
        in
           ((one,two,three), seed''')

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
byteString maxLength = int 0 maxLength `andThen` (\n -> map intsToString (list n notNull))

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

partiallyLinkedChildren : Int -> Int -> Generator ChildNode
partiallyLinkedChildren minChildren maxChildren =
    let child : Int -> Generator ChildNode
        child depth =
            if depth <= 0 then always EmptyChildNode
            else
                childNode
                `map` flashAddress
                `andMap` childFlags
                `andMap` child (depth - 1)     -- next child
                `andMap` always EmptyChildNode -- prev child, linked later
                `andMap` tripleOf byte         -- ctr
                `andMap` byteString 23         -- description
                `andMap` byteString 62         -- login
                `andMap` byteArray 32          -- password
                `andMap` pairOf byte           -- date last used
                `andMap` pairOf byte           -- date created
    in (int minChildren maxChildren) `andThen` child

linkedChildren : Int -> Int -> Generator ChildNode
linkedChildren minDepth maxDepth =
    map
        (firstChild << linkPrevChildrenReturnLast)
        (partiallyLinkedChildren minDepth maxDepth)

partiallyLinkedParents : Int -> Int -> Int -> Int -> Generator ParentNode
partiallyLinkedParents minChildren maxChildren minParents maxParents =
    let parent : Int -> Generator ParentNode
        parent depth =
          if depth <= 0 then always EmptyParentNode
          else
              parentNode
              `map` flashAddress
              `andMap` parentFlags
              `andMap` parent (depth - 1)
              `andMap` always EmptyParentNode
              `andMap` linkedChildren minChildren maxChildren
              `andMap` byteString 32
    in (int minParents maxParents) `andThen` parent

firstParentOfLinkedList : Int -> Int -> Int -> Generator ParentNode
firstParentOfLinkedList minChildren maxChildren maxParents =
    map
        (firstParent << linkPrevParentsReturnLast)
        <| partiallyLinkedParents minChildren maxChildren 1 maxParents

tests =
    let writeThenParseParentSucceeds ((ParentNode d),addr) =
            isOk (parse EmptyParentNode addr (parentToArray d))
        dataFromParse (Ok ((ParentNode d), addr)) = d
        writeThenParseParentRetains ((ParentNode d),addr) =
            parentToArray
                (dataFromParse
                    (parse EmptyParentNode addr (parentToArray d)))
                        == parentToArray d
        writeThenParseParentRetains2 ((ParentNode d),addr) =
            dataFromParse
                (parse EmptyParentNode addr (parentToArray d))
                    == {d | address <- addr}
        writeThenParseChildSucceeds (p,(ChildNode cd),addr) =
            isOk (parse p addr (childToArray cd))
        writeThenParseChildRetains ((ParentNode d),(ChildNode cd),addr) =
            .firstChild
            (dataFromParse
                (parse (ParentNode d) addr (childToArray cd)))
                    == ChildNode {cd | address <- addr}
    in simpleCheck
    [ property "- 'null term string length remains the same'"
        (\str -> Result.map String.length (nullTermString (String.length str + 3) ((stringToInts str) ++ [0, 0, 0])) == Ok (String.length str))
        (byteString 255)
    , property
        "- 'Going to lastParent and then to firstParent is the same as staying on firstParent'"
        (\p -> firstParent (lastParent p) == p)
        (firstParentOfLinkedList 0 0 10)
    , property
        "- 'foldlParents returns same node'"
        (\p -> foldlParents (\a _ -> ParentNode a) EmptyParentNode p == p)
        (firstParentOfLinkedList 0 0 10)
    , property
        "- 'foldrParents returns same node'"
        (\p -> foldrParents (\a _ -> ParentNode a) EmptyParentNode p == p)
        (map lastParent (firstParentOfLinkedList 0 0 10))
    , property
        "- 'firstParent of first parent is first parent'"
        (\p -> firstParent p == p)
        (firstParentOfLinkedList 0 0 10)
    , property "- 'lastParent of last parent is last parent'"
        (\p -> lastParent p == lastParent (lastParent p))
        (firstParentOfLinkedList 0 0 10)
    , property
        "- 'Going to last child and then to first child is the same as staying on first child'"
        (\c -> firstChild (lastChild c) == c)
        (linkedChildren 0 10)
    , property
        "- 'foldlChildren returns same node'"
        (\c -> foldlChildren (\a _ -> ChildNode a) EmptyChildNode c == c)
        (linkedChildren 0 10)
    , property
        "- 'foldrChildren returns same node'"
        (\c -> foldrChildren (\a _ -> ChildNode a) EmptyChildNode c == c)
        (map lastChild (linkedChildren 0 10))
    , property
        "- 'firstChild of first child is first child'"
        (\c -> firstChild c == c)
        (linkedChildren 0 10)
    , property "- 'lastChild of last child is last child'"
        (\c -> lastChild c == lastChild (lastChild c))
        (linkedChildren 0 10)
    , property "- 'Write parent node is 132 bytes'"
        (\(ParentNode d) -> length (parentToArray d) == 132)
        (firstParentOfLinkedList 0 0 10)
    , property "- 'Write child node is 132 bytes'"
        (\(ChildNode d) -> length (childToArray d) == 132)
        (linkedChildren 1 10)
    , property "- 'Write then parse parent'"
        writeThenParseParentSucceeds
        (map2 (,) (firstParentOfLinkedList 0 0 1) flashAddress)
    , property "- 'Write then parse parent retains'"
        writeThenParseParentRetains
        (map2 (,) (firstParentOfLinkedList 0 0 1) flashAddress)
    , property "- 'Write then parse parent retains 2'"
        writeThenParseParentRetains2
        (map2 (,) (firstParentOfLinkedList 0 0 1) flashAddress)
    , property "- 'Write then parse child'"
        writeThenParseChildSucceeds
        ((,,) `map` (firstParentOfLinkedList 0 0 1) `andMap` (linkedChildren 1 1) `andMap` flashAddress)
    , property "- 'Write then parse child retains'"
        writeThenParseChildRetains
        ((,,) `map` (firstParentOfLinkedList 0 0 1) `andMap` (linkedChildren 1 1) `andMap` flashAddress)
    ]

main = display tests
