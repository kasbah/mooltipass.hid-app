module Util where

isJust : Maybe a -> Bool
isJust x = case x of
    (Just _) -> True
    Nothing  -> False
