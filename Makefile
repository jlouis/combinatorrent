build:
	runghc Setup.lhs build

local-install:
	cabal install --prefix=$$HOME --user

haddock:
	runghc Setup.lhs haddock --executables

hlint:
	hlint src/*.hs

etags:
	hasktags --etags src/*.hs

ctags:
	hasktags --ctags src/*.hs

