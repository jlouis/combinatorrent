.PHONY: build clean rebuild local-install haddock hlint etags ctags conf dist-rebuild conf-nodebug
build:
	runghc Setup.lhs build

clean:
	runghc Setup.lhs clean

conf:
	runghc Setup.lhs configure --flags=debug --user

conf-nodebug:
	runghc Setup.lhs configure --user

rebuild: configure build

dist-rebuild: clean local-install

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

