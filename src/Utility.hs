module Utility
where

import           Data.Map.Strict               as M

removeSpaces :: String -> String
removeSpaces = Prelude.foldr f ""
  where
    f c acc | (c == ' ' || c == '\t' || c == '\r') = acc
            | otherwise                            = c : acc

(!?) :: Ord k => Map k v -> k -> Maybe v
(!?) = flip M.lookup

insertIfAbsent :: Ord k => k -> v -> Map k v -> Map k v
insertIfAbsent = M.insertWith $ flip const

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing  a = a


