.PHONY: build clean rebuild local-install haddock hlint etags ctags
build:
	runghc Setup.lhs build

clean:
	runghc Setup.lhs clean

rebuild: clean local-install

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

