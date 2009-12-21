local-install:
	cabal install --prefix=$$HOME --user

hlint:
	hlint *.hs

etags:
	hasktags --etags src/*.hs

ctags:
	hasktags --ctags src/*.hs

