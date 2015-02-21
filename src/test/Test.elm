import Check (..)
import Random (..)
import DeviceFlash (..)
import Byte (..)

import Text (plainText)

pairOf : Generator a -> Generator (a,a)
pairOf generator =
    customGenerator <| \seed ->
        let (left , seed' ) = generate generator seed
            (right, seed'') = generate generator seed'
        in
           ((left,right), seed'')

threeOf : Generator a -> Generator (a,a,a)
threeOf generator =
    customGenerator <| \seed ->
        let (one, seed' )    = generate generator seed
            (two, seed'')    = generate generator seed'
            (three, seed''') = generate generator seed''
        in
           ((one,two,three), seed''')

flashAddress : Generator FlashAddress
flashAddress = pairOf byte

ctr : Generator (Byte,Byte,Byte)
ctr = threeOf byte

byte : Generator Byte
byte = int 0 255

--parentNode : Generator ParentNode
--parentNode =
--    customGenerator <| \seed ->
--        let (address, seed') = generate flashAddress seed

--childNode : Generator ChildNode
--childNode =
--    customGenerator <| \seed ->
--        let (address, seed') = generate flashAddress seed
--            (nextChild, seed'') = generate childNode seed'
--            (ctr, seed''')  = generate ctr seed''
--            (descr, seed'''') = generate string seed

ints : Generator (List Byte)
ints = list 132 byte

tests =
  simpleCheck [
    property "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
  ]

main = display tests
