import Check (..)
import Random (..)

import Text (plainText)

tests =
  simpleCheck [
    property "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
  ]

main = display tests
