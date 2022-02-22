{-# LANGUAGE RecordWildCards #-}

module CFGFuncs where

import CFGData (CNFRule (RuleN, _leftN, _rightN), ChomskyNormalForm (CNF, nonTerminalsN, rulesN, startingSymbolN, terminalsN), ContextFreeGrammar (CFG, nonTerminals, rules, startingSymbol, terminals), Rule (Rule, _left, _right), Symbol, Symbols)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isLower, isUpper)
import Errors (CustomError (InvalidCFG))
import Lib (allUnique, filterEmptySublists, unique)

convertToCNF :: ContextFreeGrammar -> ChomskyNormalForm
convertToCNF CFG {..} =
  CNF
    { terminalsN = terminals,
      nonTerminalsN = unique (map (: []) nonTerminals ++ filterEmptySublists (snd newRules)),
      startingSymbolN = startingSymbol,
      rulesN = fst newRules
    }
  where
    newRules = foldr (\rule acc -> joinTuples acc (convertRuleToCNF rule)) ([], [[]]) rules

-- join lists in tuples

joinTuples :: Bifunctor p => ([a1], [a2]) -> p [a1] [a2] -> p [a1] [a2]
joinTuples tup1 = bimap (fst tup1 ++) (snd tup1 ++)

-- process single rule conversion
convertRuleToCNF :: Rule -> ([CNFRule], [Symbols])
convertRuleToCNF rule@Rule {..}
  | inCNF rule = ([RuleN {_leftN = [_left], _rightN = _right}], [[]])
  | twoSymbols _right = processTwoSymbols ruleN
  | otherwise = processMultipleSymbols ruleN
  where
    ruleN = RuleN {_leftN = [_left], _rightN = _right}

-- continueSimplification:: CNFRule -> ([CNFRule], [Symbols])
-- continueSimplification rule@RuleN{..}
--   | twoSymbols _rightN = processTwoSymbols rule
--   | otherwise = processMultipleSymbols rule
-- -- convertRuleToCNF RuleN {_leftN=tail _right, _rightN= tail (tail _right) }

-- Atleast one
twoSymbols :: Symbols -> Bool
twoSymbols _right = length _right == 2

processMultipleSymbols :: CNFRule -> ([CNFRule], [Symbols])
processMultipleSymbols rule@RuleN {..} =
  if length _rightN == 2
    then processTwoSymbols rule
    else
      joinTuples
        ( if isLower (head _rightN)
            then
              ( [ RuleN {_leftN = _leftN, _rightN = [head _rightN] ++ "'<" ++ tail _rightN ++ ">"},
                  RuleN {_leftN = head _rightN : "'", _rightN = [head _rightN]}
                ],
                (head _rightN : "'") : ["<" ++ tail _rightN ++ ">"]
              )
            else ([RuleN {_leftN = _leftN, _rightN = [head _rightN] ++ "<" ++ tail _rightN ++ ">"}], ["<" ++ tail _rightN ++ ">"])
        )
        (processMultipleSymbols RuleN {_leftN = "<" ++ tail _rightN ++ ">", _rightN = tail _rightN})

--Process tw
processTwoSymbols :: CNFRule -> ([CNFRule], [Symbols])
processTwoSymbols RuleN {..} =
  ( RuleN {_leftN = _leftN, _rightN = fst processRules} :
      [RuleN {_leftN = newNonTerm, _rightN = [head newNonTerm]} | newNonTerm <- filterEmptySublists (snd processRules)],
    snd processRules
  )
  where
    processRules =
      foldr (\char acc -> joinTuples (if isLower char then (char : "'", [char : "'"]) else ([char], [])) acc) ([], [[]]) _rightN

-- Check whatever rule is already in CNF
inCNF :: Rule -> Bool
inCNF rule = singleTerminal rule || twoNonTerminals rule

-- Single terminal on ther right side - A-> a
singleTerminal :: Rule -> Bool
singleTerminal Rule {..} = length _right == 1 && isLower (head _right)

-- Two nonterminals on ther right side - A-> BB
twoNonTerminals :: Rule -> Bool
twoNonTerminals Rule {..} = length _right == 2 && all isUpper _right

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