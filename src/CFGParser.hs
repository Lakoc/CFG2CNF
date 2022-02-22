{-# LANGUAGE RecordWildCards #-}

module CFGParser where

import CFGData (ContextFreeGrammar (CFG, nonTerminals, rules, startingSymbol, terminals), Rule (Rule, _left, _right), Rules, Symbols)
import Data.Char (isLower, isUpper)
import Errors (CustomError (InvalidCFG))
import GHC.Base ()
import Lib (allUnique)
import Text.Parsec (ParseError, char, endBy, letter, many1, newline, parse, satisfy, sepBy1, string)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser)

-- Main parser
parseCFG :: String -> Either ParseError ContextFreeGrammar
parseCFG str = case parse cfgParser "" str of
  Left err -> Left err
  Right xs -> Right xs

cfgParser :: Parser ContextFreeGrammar
cfgParser =
  CFG <$> nonTerminalsP <* newline
    <*> terminalsP <* newline
    <*> satisfy isUpper <* newline -- startingSymbol
    <*> rulesP <* eof

nonTerminalsP :: Parser Symbols
nonTerminalsP = sepBy1 (satisfy isUpper) commaP

terminalsP :: Parser Symbols
terminalsP = sepBy1 (satisfy isLower) commaP

rulesP :: Parser Rules
rulesP = endBy ruleP newlineP

ruleP :: Parser Rule
ruleP = Rule <$> satisfy isUpper <* arrowP <*> many1 letter

commaP :: Parser Char
commaP = char ','

arrowP :: Parser String
arrowP = string "->"

newlineP :: Parser Char
newlineP = char '\n'

-- Validate context free grammar
validateCFG :: ContextFreeGrammar -> Either CustomError ContextFreeGrammar
validateCFG cfg@CFG {..} = if validGram then Right cfg else Left InvalidCFG
  where
    validGram =
      startingSymbol `elem` nonTerminals -- startingSymbol is defined in nonTerminals
        && allUnique nonTerminals -- check if it's set
        && allUnique terminals -- check if it's set
        && all validRule rules
      where
        validRule Rule {..} =
          _left `elem` nonTerminals -- left side of rule must be nonTerminal
            && all (\symbol -> symbol `elem` terminals || symbol `elem` nonTerminals) _right -- symbols on the right side belongs to terminals or nonTerminals