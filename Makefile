all:
	ghc --make app/Main.hs -i src/Errors.hs src/CFGData.hs src/CFGParser.hs src/Lib.hs -o flp21-fun -Wall -fforce-recomp