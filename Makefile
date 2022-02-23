all:
ghc --make app/Main.hs -i src/Errors.hs src/SimplifyCFG.hs src/GrammarTypes.hs src/ValidateCFG.hs src/CFG2CNF.hs src/CFGParser.hs src/Lib.hs -o flp21-fun -Wall -fforce-recomp