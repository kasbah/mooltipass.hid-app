module Byte where

-- Elm standard library
import String
import List
import Char
import Result


-- local source
import Util (..)

{-| A byte is just an Int really. -}
type alias Byte = Int

{-| We make sure values are between 0 and 255 when we convert to a byte. -}
toByte : Int -> Result Error Byte
toByte x = if (x >= 0) && (x < 256)
           then Ok x
           else Err "Invalid int given to byte conversion"

{-| A bytestring is just a string really -}
type alias ByteString = String

{-| We make sure the length (size) is right and values are between 0 and 255
    when we convert Ints to a bytestring. -}
toByteString : Int -> List Int -> Result Error ByteString
toByteString size ints =
    Result.map intsToString (toByteArray size ints)

intsToString : List Int -> String
intsToString = String.fromList << (List.map Char.fromCode)

nullTermString : Int -> List Int -> Result Error ByteString
nullTermString maxSize ints =
    Result.map (intsToString << truncateNull) (toByteArray maxSize ints)

{-| We make sure values are between 0 and 255 when we convert a String to a
    bytestring. -}
byteString : String -> Result Error ByteString
byteString s =
    toByteString (String.length s)
    <| List.map Char.toCode (String.toList s)

{-| A byte array is just a list of 'Byte' -}
type alias ByteArray = List Byte

{-| We make sure the length (size) is right and values are between 0 and 255
    when we convert Ints to a byte array. -}
toByteArray : Int -> List Int -> Result Error ByteArray
toByteArray size ints =
    if | size > List.length ints -> Err "size is greater than data to convert to bytes"
       | size <= 0 -> Err "Size is less than or equal to zero, while converting to bytes"
       | otherwise ->
         if List.foldr (\int b -> b && int >= 0 && int < 256) True
                (List.take size ints)
         then Ok <| List.take size ints
         else Err "Invalid char given to byte conversion (unicode?)"

stringToInts : String -> List Int
stringToInts s  = List.map Char.toCode (String.toList s)

--stringToByteArray :: String -> Maybe ByteArray
--stringToByteArray s  = List.map Char.toCode (String.toList s)

{-| This is (LSB, MSB) -}
type alias FlashAddress = (Byte, Byte)

{-| Our error type is just a string that explains the error. -}
type alias Error = String

null : FlashAddress
null = (0,0)
