all:
	ghc --make *.hs

hlint:
	hlint *.hs
