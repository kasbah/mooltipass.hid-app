module Byte where

-- Elm standard library
import String
import List
import Char

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
    when we convert to a bytestring. -}
toByteString : Int -> List Int -> Result Error ByteString
toByteString size ints =
    if size > List.length ints || size <= 0
    then Err "Invalid size to convert to bytestring"
    else if List.foldr (\int b -> b && int >= 0 && int < 256) True
                (List.take size ints)
         then Ok <| String.fromList (List.map Char.fromCode (List.take size ints))
         else Err "Invalid char given to byte conversion (unicode?)"

{-| We make sure values are between 0 and 255 when we convert to a bytestring.
    -}
byteString : String -> Result Error ByteString
byteString s =
    toByteString (String.length s)
    <| List.map Char.toCode (String.toList s)

{-| Our error type is just a string that explains the error. -}
type alias Error = String
