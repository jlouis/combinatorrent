all:
	ghc -Wall --make -threaded *.hs

hlint:
	hlint *.hs

etags:
	hasktags --etags *.hs

ctags:
	hasktags --ctags *.hs

clean:
	rm *.hi *.o
