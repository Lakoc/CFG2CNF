{-# LANGUAGE RecordWildCards #-}

module CFGData where

import Lib (joinWithSep, joinWithSepStrings)

type Symbol = Char

type Symbols = [Symbol]

type Rules = [Rule]

data Rule = Rule
  { _left :: Symbol,
    _right :: Symbols
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

-- Chomsky Normal Form Grammar type
data CNFRule = RuleN
 {
  _leftN :: Symbols,
  _rightN :: Symbols
 }

instance Show CNFRule where
  show RuleN {..} = _leftN ++ "->" ++ _rightN

data ChomskyNormalForm = CNF
  { nonTerminalsN :: [Symbols],
    terminalsN :: Symbols,
    startingSymbolN :: Symbol,
    rulesN :: [CNFRule]
  }
-- Split each entity by lines
instance Show ChomskyNormalForm where
  show CNF {..} =
    unlines $
      [joinWithSepStrings ','nonTerminalsN]
        ++ [joinWithSep ',' terminalsN]
        ++ [[startingSymbolN]]
        ++ map show rulesN


