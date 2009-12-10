all:
	ghc -Wall --make *.hs

hlint:
	hlint *.hs

clean:
	rm *.hi *.o
