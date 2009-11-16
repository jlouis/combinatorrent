all:
	ghc -Wall --make *.hs

hlint:
	hlint *.hs
