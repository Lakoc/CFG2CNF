module Lib where

import Data.List (group, sort)

safeInit :: [a] -> [a]
safeInit [] = []
safeInit [_] = []
safeInit (x:xs) = x : safeInit xs

-- Add to the list separator between every element 
joinWithSep :: Foldable t => a -> t a -> [a]
joinWithSep sep = safeInit . foldr (\x acc -> x : sep : acc) []

joinWithSepStrings :: Foldable t => a -> t [a] -> [a]
joinWithSepStrings sep = safeInit . foldr (\x acc -> x ++ [sep] ++ acc) []

filterEmptySublists :: Foldable t => [t a] -> [t a]
filterEmptySublists lists = [ i | i <- lists, not (null i) ]

-- Sort List and group by elements
sg :: Ord a => [a] -> [[a]]
sg = group . sort

-- Check whatever all elements are unique in List
allUnique :: Ord a => [a] -> Bool
allUnique = all ((==) 1 . length) . sg

-- Remove duplicates from list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (filter (x /=) xs)
