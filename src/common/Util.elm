module Util where

import List (filterMap)

isJust : Maybe a -> Bool
isJust x = case x of
    (Just _) -> True
    Nothing  -> False

justs : List (Maybe a) -> List a
justs = filterMap (\x -> x)

mapOnce : (a -> Maybe a) -> List a -> List a
mapOnce f (x::xs) = case f x of
        Nothing -> x :: if xs /= [] then mapOnce f xs else []
        Just y  -> y :: xs

replace : List a -> a -> a -> List a
replace items old new =
    let check item = if item == old then Just new else Nothing
    in mapOnce check items
