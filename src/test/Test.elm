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

genChildNode : Int -> Generator ChildNode
genChildNode depth =
    if depth <= 0 then always EmptyChildNode
    else
        childNode
        `map` flashAddress
        `andMap` pairOf byte
        `andMap` genChildNode (depth - 1)
        `andMap` always EmptyChildNode
        `andMap` tripleOf byte
        `andMap` byteString 32
        `andMap` byteString 32
        `andMap` byteArray 32
        `andMap` pairOf byte
        `andMap` pairOf byte

genChildNodeMaxDepth : Int -> Generator ChildNode
genChildNodeMaxDepth maxDepth =
    (int 0 maxDepth) `andThen` genChildNode


genParentNode : Int -> Int -> Generator ParentNode
genParentNode maxChildren depth =
    if depth <= 0 then always EmptyParentNode
    else
        parentNode
        `map` flashAddress
        `andMap` pairOf byte
        `andMap` genParentNode maxChildren (depth - 1)
        `andMap` always EmptyParentNode
        `andMap` genChildNodeMaxDepth maxChildren
        `andMap` byteString 32

genParentNodeMaxDepth : Int -> Int -> Generator ParentNode
genParentNodeMaxDepth maxChildren maxDepth =
    (int 0 maxDepth) `andThen` genParentNode maxChildren


ints : Generator (List Byte)
ints = list 132 byte

tests =
  simpleCheck [
    property "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
  ]

main = display tests
