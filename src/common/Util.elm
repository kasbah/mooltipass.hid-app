module Util where

import List (filterMap)

isJust : Maybe a -> Bool
isJust x = case x of
    (Just _) -> True
    Nothing  -> False

justs : List (Maybe a) -> List a
justs = filterMap (\x -> x)
