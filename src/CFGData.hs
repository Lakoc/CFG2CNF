{-# LANGUAGE RecordWildCards #-}

module CFGData where

import Lib (joinWithSep)

type Symbol = Char

type Symbols = [Symbol]

type Rules = [Rule]

data Rule = Rule
  { _left :: Symbol,
    _right :: [Symbol]
  }

instance Show Rule where
  show Rule {..} = [_left] ++ "->" ++ _right

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
      [joinWithSep ',' nonTerminals]
        ++ [joinWithSep ',' terminals]
        ++ [[startingSymbol]]
        ++ map show rules
