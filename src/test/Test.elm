import Check (..)
import Random (..)
import DeviceFlash (..)
import Byte (..)

import Text (..)

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
flashAddress = pairOf byte

ctr : Generator (Byte,Byte,Byte)
ctr = tripleOf byte

byte : Generator Byte
byte = int 0 255

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

partiallyLinkedChildNode : Int -> Generator ChildNode
partiallyLinkedChildNode depth =
    if depth <= 0 then always EmptyChildNode
    else
        childNode
        `map` flashAddress
        `andMap` pairOf byte
        `andMap` partiallyLinkedChildNode (depth - 1) -- next child
        `andMap` always EmptyChildNode -- prev child
        `andMap` tripleOf byte
        `andMap` byteString 32
        `andMap` byteString 32
        `andMap` byteArray 32
        `andMap` pairOf byte
        `andMap` pairOf byte

partiallyLinkedChildNodeMaxDepth : Int -> Generator ChildNode
partiallyLinkedChildNodeMaxDepth maxDepth =
    (int 0 maxDepth) `andThen` partiallyLinkedChildNode

partiallyLinkedParentNode : Int -> Int -> Generator ParentNode
partiallyLinkedParentNode maxChildren depth =
    if depth <= 0 then always EmptyParentNode
    else
        parentNode
        `map` flashAddress
        `andMap` pairOf byte
        `andMap` partiallyLinkedParentNode maxChildren (depth - 1)
        `andMap` always EmptyParentNode
        `andMap` partiallyLinkedChildNodeMaxDepth maxChildren
        `andMap` byteString 32

partiallyLinkedParentNodeMaxDepth : Int -> Int -> Generator ParentNode
partiallyLinkedParentNodeMaxDepth maxChildren maxDepth =
    (int 0 maxDepth) `andThen` partiallyLinkedParentNode maxChildren

doublyLinkedList : Generator ParentNode
doublyLinkedList =
    let linkChildren p = case p of
        EmptyParentNode -> EmptyParentNode
        ParentNode d    ->
            ParentNode {d | firstChild <- firstChild (linkPrevChildrenReturnLast d.firstChild)}
    in map (mapParents linkChildren)
        <| map (firstParent << linkPrevParentsReturnLast)
        <| partiallyLinkedParentNodeMaxDepth 50 50

ints : Generator (List Byte)
ints = list 132 byte

tests =
  simpleCheck [
    property "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
  ]

main = display tests
