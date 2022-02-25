{-
  Project: VUT FIT FLP BKG-2-CNF
  Author: Alexander Polok <xpolok03@stud.fit.vutbr.cz>
  Date: 25.2.2022
-}
{-# LANGUAGE RecordWildCards #-}

module CFG2CNF where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isLower, isUpper)
import GrammarTypes (CNFRule (RuleN, _leftN, _rightN), ChomskyNormalForm (CNF, nonTerminalsN, rulesN, startingSymbolN, terminalsN), ContextFreeGrammar (CFG, nonTerminals, rules, startingSymbol, terminals), Rule (Rule, _left, _right), Symbols)
import Lib (filterEmptySublists, unique)

-- Convert CFG -> CNF
convertToCNF :: ContextFreeGrammar -> ChomskyNormalForm
convertToCNF CFG {..} =
  CNF
    { terminalsN = terminals,
      nonTerminalsN = unique (map (: []) nonTerminals ++ filterEmptySublists (snd newRulesNonTerminals)), -- join old with new nonTerminals
      startingSymbolN = startingSymbol,
      rulesN = unique (fst newRulesNonTerminals) -- generate new rules, that satisfy CNF condition
    }
  where
    newRulesNonTerminals = foldr (\rule acc -> joinTuples acc (convertRuleToCNF rule)) ([], [[]]) rules -- iterate over each rule and convert it to CNF

-- Join tuples element-wise
joinTuples :: Bifunctor p => ([a1], [a2]) -> p [a1] [a2] -> p [a1] [a2]
joinTuples tup1 = bimap (fst tup1 ++) (snd tup1 ++)

-- Process single rule conversion
convertRuleToCNF :: Rule -> ([CNFRule], [Symbols])
convertRuleToCNF rule@Rule {..}
  | inCNF rule = ([RuleN {_leftN = [_left], _rightN = _right}], [[]]) -- rule is already in CNF
  | twoSymbols _right = processTwoSymbols ruleN -- two symbols could be processed without recursion
  | otherwise = processMultipleSymbols ruleN -- start recursive generation
  where
    ruleN = RuleN {_leftN = [_left], _rightN = _right}

-- Check whatever right side of rule contains 2 symbols
twoSymbols :: Symbols -> Bool
twoSymbols _right = length _right == 2

-- Recursive funtion, that generate rules in CNF
processMultipleSymbols :: CNFRule -> ([CNFRule], [Symbols])
processMultipleSymbols rule@RuleN {..} =
  if length _rightN == 2
    then processTwoSymbols rule -- no futher rucursion is needed
    else
      joinTuples
        ( if isLower (head _rightN)
            then -- terminal is first on the right side -> thus generate two new rules and new nonTerminal

              ( [ RuleN {_leftN = _leftN, _rightN = [head _rightN] ++ "'<" ++ tail _rightN ++ ">"},
                  RuleN {_leftN = head _rightN : "'", _rightN = [head _rightN]}
                ],
                (head _rightN : "'") : ["<" ++ tail _rightN ++ ">"] -- new nonTerminal
              )
            else ([RuleN {_leftN = _leftN, _rightN = [head _rightN] ++ "<" ++ tail _rightN ++ ">"}], ["<" ++ tail _rightN ++ ">"]) -- generate single rule and nonTerminal
        )
        (processMultipleSymbols RuleN {_leftN = "<" ++ tail _rightN ++ ">", _rightN = tail _rightN}) -- further recursion is needed

-- Process two symbols, generate new rules, rule is not in CNF so atleas 2 rules will be created, generate new nonTerminals
processTwoSymbols :: CNFRule -> ([CNFRule], [Symbols])
processTwoSymbols RuleN {..} =
  ( RuleN {_leftN = _leftN, _rightN = fst processRules} : -- New rule, right side nonTerminals only
      [RuleN {_leftN = newNonTerm, _rightN = [head newNonTerm]} | newNonTerm <- filterEmptySublists (snd processRules)], -- generate new nonTerminals and their rules
    snd processRules
  )
  where
    processRules =
      foldr (\char acc -> joinTuples (if isLower char then (char : "'", [char : "'"]) else ([char], [])) acc) ([], [[]]) _rightN -- Generate tuples with new nonterminals and right side of rules

-- Check whatever rule is already in CNF
inCNF :: Rule -> Bool
inCNF rule = singleTerminal rule || twoNonTerminals rule

-- Single terminal on ther right side - A-> a
singleTerminal :: Rule -> Bool
singleTerminal Rule {..} = length _right == 1 && isLower (head _right)

-- Two nonterminals on ther right side - A-> BB
twoNonTerminals :: Rule -> Bool
twoNonTerminals Rule {..} = length _right == 2 && all isUpper _right