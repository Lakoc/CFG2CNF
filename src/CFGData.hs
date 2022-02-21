{-# LANGUAGE RecordWildCards #-}

module CFGData where

import Data.List (intercalate)

type Symbol = Char

type Symbols = [Symbol]

data Rule = Rule
  { left :: Symbol,
    right :: [Symbol]
  }

instance Show Rule where
  show Rule {..} = [left] ++ "->" ++ right

type Rules = [Rule]

data ContextFreeGrammar = Gram
  { terminals :: Symbols,
    nonTerminals :: Symbols,
    rules :: Rules,
    startingSymbol :: Symbol
  }

instance Show ContextFreeGrammar where
  show Gram {..} =
    unlines $
      [intercalate "," $ map (: []) nonTerminals]
        ++ [intercalate "," $ map (: []) terminals]
        ++ [[startingSymbol]]
        ++ map show rules
