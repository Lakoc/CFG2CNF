{-# LANGUAGE RecordWildCards #-}

module CFGFuncs where

import CFGData (ContextFreeGrammar (CFG, nonTerminals, rules, startingSymbol, terminals), Rule (Rule, _left, _right), Symbol)
import Data.Char (isUpper)
import Errors (CustomError (InvalidCFG))
import Lib (allUnique)



--
-- Simplification of the context free grammar
--

-- Recursively generate not simple rules with specified nonTerminal on the left side 
generateApplicableRules :: Symbol -> [Rule] -> [Rule]
generateApplicableRules nonTerminal rules
  | all complexRule nonTerminalOnLeft = nonTerminalOnLeft -- All rules with current nonTerminal are nonSimple
  | otherwise =
    foldr
      ( \derivableNonTerminal acc ->
          acc ++ complexRulesSameLeft -- add all non simple rules
            ++ map
              (`replaceRuleLeft` nonTerminal) -- replace their left sides to get new rules
              (generateApplicableRules derivableNonTerminal rules) -- get recursively complex rules derived from nonTerminals reachable by simple rule of the current nonTerminal
      )
      []
      reachableNonTerminals
  where
    nonTerminalOnLeft = rulesSameLeft nonTerminal rules -- all rules that has current nonTerminal on the left side
    complexRulesSameLeft = filter complexRule nonTerminalOnLeft -- all rules that are not simple in `nonTerminalOnLeft`
    reachableNonTerminals = getRightNonTerminals (filter simpleRule nonTerminalOnLeft) -- all nonTerminals that are on the right side of simple `nonTerminalOnLeft` rules

-- Generate all complex rules
generateApplicableRulesAll :: [Symbol] -> [Rule] -> [Rule]
generateApplicableRulesAll nonTerminals rules =
  foldr
    (\nonTerminal acc -> acc ++ generateApplicableRules nonTerminal rules)
    []
    nonTerminals

-- Return symbols on the right side of simple rules
getRightNonTerminals :: [Rule] -> [Symbol]
getRightNonTerminals = map (head . _right)

-- Replace left side of the rule with new nonTerminal
replaceRuleLeft :: Rule -> Symbol -> Rule
replaceRuleLeft Rule {..} newLeft = Rule {_left = newLeft, _right = _right}

-- Rules that have provided nonTerminal on left side
rulesSameLeft :: Symbol -> [Rule] -> [Rule]
rulesSameLeft nonTerminal = filter (`ruleSameLeft` nonTerminal)

-- Check whatever provided nonTerminal is on the right side of rule
ruleSameLeft :: Rule -> Symbol -> Bool
ruleSameLeft Rule {..} nonTerminal = nonTerminal == _left

-- Check whateaver rule is not simple - example of such rule A->a or A->Aa
complexRule :: Rule -> Bool
complexRule = not . simpleRule

-- Check whateaver rule is not simple - example A->B
simpleRule :: Rule -> Bool
simpleRule Rule {..} = length _right == 1 && isUpper (head _right)

-- Create new ContextFreeGrammar with simplified rules
simplifyCFG :: ContextFreeGrammar -> ContextFreeGrammar
simplifyCFG CFG {..} = CFG {nonTerminals = nonTerminals, terminals = terminals, startingSymbol = startingSymbol, rules = rules'}
  where
    rules' = generateApplicableRulesAll nonTerminals rules



--
-- Validation of the context free grammar
--

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