.PHONY: test build clean rebuild local-install haddock hlint tags conf dist-rebuild conf-nodebug conf-ts
build:
	runghc Setup.lhs build

clean:
	runghc Setup.lhs clean

test: build
	runghc Setup.lhs test

conf:
	runghc Setup.lhs configure --flags="debug" --user --enable-library-profiling --enable-executable-profiling

conf-threaded:
	runghc Setup.lhs configure --flags="debug threaded" --user --enable-library-profiling --enable-executable-profiling

conf-ts:
	runghc Setup.lhs configure --flags="debug threadscope" --user

conf-nodebug:
	runghc Setup.lhs configure --user

rebuild: configure build

dist-rebuild: clean local-install

local-install:
	cabal install --prefix=$$HOME --user

haddock:
	runghc Setup.lhs haddock --executables

hlint:
	hlint -r --cpp-define='__GLASGOW_HASKELL__=612' src

tags:
	hasktags --both $$(find src -type f -name '*.*hs')

