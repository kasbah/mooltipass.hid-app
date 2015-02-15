module Util where

import List (..)

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

replaceFirst : a -> a -> List a -> List a
replaceFirst old new items =
    let check item = if item == old then Just new else Nothing
    in mapOnce check items

stripNothing : List (Maybe a) -> List (Maybe a)
stripNothing =
    foldr (\x z -> if z == [] && x == Nothing then z else x::z) []
