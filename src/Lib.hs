module Lib where

joinWithSep :: Foldable t => a -> t a -> [a]
joinWithSep sep = foldr (\x acc -> x : sep : acc) []
