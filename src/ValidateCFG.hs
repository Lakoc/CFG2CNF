{-# LANGUAGE RecordWildCards #-}
module ValidateCFG where
import Errors ( CustomError(InvalidCFG) )
import Lib ( allUnique )
import GrammarTypes (ContextFreeGrammar (CFG, startingSymbol, nonTerminals, terminals, rules), Rule (Rule, _left, _right))

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