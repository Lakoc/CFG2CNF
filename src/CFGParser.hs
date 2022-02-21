module CFGParser where

import CFGData (ContextFreeGrammar (Gram, nonTerminals, rules, startingSymbol, terminals), Rule (Rule, left, right))

parseCFG :: p -> ContextFreeGrammar
parseCFG a = Gram {nonTerminals = ['A', 'B'], startingSymbol = 'A', terminals = ['a', 'b'], rules = [Rule {left = 'A', right = "aBasd"}, Rule {left = 'B', right = "aBasd"}]}