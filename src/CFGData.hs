{-# LANGUAGE RecordWildCards #-}

module CFGData where

import Lib (joinWithSep)

type Symbol = Char

type Symbols = [Symbol]

type Rules = [Rule]

data Rule = Rule
  { left :: Symbol,
    right :: [Symbol]
  }

instance Show Rule where
  show Rule {..} = [left] ++ "->" ++ right

data ContextFreeGrammar = CFG
  { nonTerminals :: Symbols,
    terminals :: Symbols,
    startingSymbol :: Symbol,
    rules :: Rules
  }

-- Split each entity by lines
instance Show ContextFreeGrammar where
  show CFG {..} =
    unlines $
      [init (joinWithSep ',' nonTerminals)]
        ++ [init (joinWithSep ',' terminals)]
        ++ [[startingSymbol]]
        ++ map show rules
