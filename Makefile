.PHONY: test build clean rebuild local-install haddock hlint tags conf dist-rebuild conf-nodebug conf-ts
build:
	runghc Setup.lhs build

clean:
	runghc Setup.lhs clean

test: build
	runghc Setup.lhs test

conf:
	runghc Setup.lhs configure --user --enable-library-profiling --enable-executable-profiling --enable-optimization=2

conf-debug:
	runghc Setup.lhs configure --flags="debug" --user --enable-library-profiling --enable-executable-profiling --enable-optimization

conf-t:
	runghc Setup.lhs configure --flags="threaded" --user --enable-library-profiling --enable-optimization=2

conf-hpc:
	runghc Setup.lhs configure --ghc-options=-fhpc --user --enable-library-profiling --enable-executable-profiling --enable-optimization

conf-no-opt:
	runghc Setup.lhs configure --flags="debug" --user --enable-library-profiling --enable-executable-profiling --enable-optimization=0

conf-ts:
	runghc Setup.lhs configure --flags="debug threadscope" --user

rebuild: configure build

dist-rebuild: clean local-install

local-install:
	cabal install --prefix=$$HOME --user

haddock:
	runghc Setup.lhs haddock --executables

hlint:
	hlint -r src

tags:
	hothasktags $$(find src -type f -name '*.*hs') > $@

