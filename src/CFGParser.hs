module CFGParser where

import GrammarTypes (ContextFreeGrammar (CFG ), Rule (Rule), Rules, Symbols)
import Data.Char (isLower, isUpper)
import GHC.Base ()
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