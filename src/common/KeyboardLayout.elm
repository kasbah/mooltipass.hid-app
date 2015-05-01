module KeyboardLayout where

import List
import Maybe

defaultKeyboard : Int
defaultKeyboard = snd (List.head allKeyboards)

allKeyboards : List (String, Int)
allKeyboards =
    [ ("EN_US", 0x92)
    , ("FR_FR", 0x93)
    , ("ES_ES", 0x94)
    , ("DE_DE", 0x95)
    , ("ES_AR", 0x96)
    , ("EN_AU", 0x97)
    , ("FR_BE", 0x98)
    , ("PO_BR", 0x99)
    , ("EN_CA", 0x9A)
    , ("CZ_CZ", 0x9B)
    , ("DA_DK", 0x9C)
    , ("FI_FI", 0x9D)
    , ("HU_HU", 0x9E)
    , ("IS_IS", 0x9F)
    , ("IT_IT", 0xA0)
    , ("NL_NL", 0xA1)
    , ("NO_NO", 0xA2)
    , ("PO_PO", 0xA3)
    , ("RO_RO", 0xA4)
    , ("SL_SL", 0xA5)
    , ("FRDE_CH", 0xA6)
    , ("EN_UK", 0xA7)
    ]

keyboardToInt : String -> Maybe Int
keyboardToInt s = Maybe.map snd <| findWith fst s allKeyboards

keyboardFromInt : Int -> Maybe String
keyboardFromInt i = Maybe.map fst <| findWith snd i allKeyboards

findWith : (a -> b) -> b -> List a -> Maybe a
findWith f target l = case l of
    []      -> Nothing
    (x::xs) -> if f x == target then Just x else findWith f target xs
