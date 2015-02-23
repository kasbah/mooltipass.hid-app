import Check (..)
import Random (..)
import DeviceFlash (..)
import Byte (..)

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
flashAddress = pairOf (int 1 255) -- (0,0) is null

ctr : Generator (Byte,Byte,Byte)
ctr = tripleOf byte

byte : Generator Byte
byte = int 0 255

parentFlags : Generator (Byte, Byte)
parentFlags = map (\(x,b) -> (x,b `and` 0x3F)) (pairOf byte)

childFlags : Generator (Byte, Byte)
childFlags = map (\(x,b) -> (x,b `and` 0x3F `or` 0x40)) (pairOf byte)

byteArray : Int -> Generator ByteArray
byteArray maxLength = list maxLength byte

byteString : Int -> Generator ByteString
byteString maxLength = map intsToString (byteArray maxLength)

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

partiallyLinkedChildren : Int -> Generator ChildNode
partiallyLinkedChildren maxChildren =
    let child : Int -> Generator ChildNode
        child depth =
            if depth <= 0 then always EmptyChildNode
            else
                childNode
                `map` flashAddress
                `andMap` childFlags
                `andMap` child (depth - 1) -- next child
                `andMap` always EmptyChildNode -- prev child
                `andMap` tripleOf byte
                `andMap` byteString 32
                `andMap` byteString 32
                `andMap` byteArray 32
                `andMap` pairOf byte
                `andMap` pairOf byte
    in (int 0 maxChildren) `andThen` child

linkedChildren : Int -> Generator ChildNode
linkedChildren maxDepth =
    map
        (firstChild << linkPrevChildrenReturnLast)
        (partiallyLinkedChildren maxDepth)

partiallyLinkedParents : Int -> Int -> Generator ParentNode
partiallyLinkedParents maxChildren maxParents =
    let parent : Int -> Generator ParentNode
        parent depth =
          if depth <= 0 then always EmptyParentNode
          else
              parentNode
              `map` flashAddress
              `andMap` parentFlags
              `andMap` parent (depth - 1)
              `andMap` always EmptyParentNode
              `andMap` linkedChildren maxChildren
              `andMap` byteString 32
    in (int 0 maxParents) `andThen` parent

firstParentOfLinkedList : Int -> Int -> Generator ParentNode
firstParentOfLinkedList maxChildren maxParents =
    map
        (firstParent << linkPrevParentsReturnLast)
        <| partiallyLinkedParents maxChildren maxParents

ints : Generator (List Byte)
ints = list 132 byte

tests =
  simpleCheck
    [ property
        "- 'Going to lastParent and then to firstParent is the same as staying on firstParent'"
        (\p -> firstParent (lastParent p) == p)
        (firstParentOfLinkedList 0 10)
    , property
        "- 'foldlParents returns first parent'"
        (\p -> foldlParents (\a _ -> ParentNode a) EmptyParentNode p == p)
        (firstParentOfLinkedList 0 10)
    , property
        "- 'firstParent of first parent is first parent'"
        (\p -> firstParent p == p)
        (firstParentOfLinkedList 0 10)
    , property "- 'lastParent of last parent is last parent'"
        (\p -> lastParent p == lastParent (lastParent p))
        (firstParentOfLinkedList 0 10)
    , property
        "- 'Going to last child and then to first child is the same as staying on first child'"
        (\c -> firstChild (lastChild c) == c)
        (linkedChildren 10)
    , property
        "- 'foldlChildren returns first child'"
        (\c -> foldlChildren (\a _ -> ChildNode a) EmptyChildNode c == c)
        (linkedChildren 10)
    , property
        "- 'firstChild of first child is first child'"
        (\c -> firstChild c == c)
        (linkedChildren 10)
    , property "- 'lastChild of last child is last child'"
        (\c -> lastChild c == lastChild (lastChild c))
        (linkedChildren 10)
    --, property "- 'write then parse of single parent retains'"
    --    (\p -> parse
    --    (firstParentOfLinkedList 0 10)
    ]

main = display tests
