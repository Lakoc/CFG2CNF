module Lib where

import Data.List (group, sort)

-- Add to the list separator between every element 
joinWithSep :: Foldable t => a -> t a -> [a]
joinWithSep sep = init . foldr (\x acc -> x : sep : acc) []

-- Sort List and group by elements
sg :: Ord a => [a] -> [[a]]
sg = group . sort

-- Check whatever all elements are unique in List
allUnique :: Ord a => [a] -> Bool
allUnique = all ((==) 1 . length) . sg
