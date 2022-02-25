{-
  Project: VUT FIT FLP BKG-2-CNF
  Author: Alexander Polok <xpolok03@stud.fit.vutbr.cz>
  Date: 25.2.2022
-}

module CFGParser where

import Data.Char (isLower, isUpper)
import GHC.Base ()
import GrammarTypes (ContextFreeGrammar (CFG), Rule (Rule), Rules, Symbols)
import Text.Parsec
  ( ParseError,
    char,
    eof,
    letter,
    many1,
    newline,
    parse,
    satisfy,
    sepBy,
    sepBy1,
    sepEndBy,
    string,
  )
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
terminalsP = sepBy (satisfy isLower) commaP

rulesP :: Parser Rules
rulesP = sepEndBy ruleP newline

ruleP :: Parser Rule
ruleP = Rule <$> satisfy isUpper <* arrowP <*> many1 letter

commaP :: Parser Char
commaP = char ','

arrowP :: Parser String
arrowP = string "->"

-- newlineP :: Parser Char
-- newlineP = char '\n'