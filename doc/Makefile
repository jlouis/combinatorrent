.PHONY: all

all: processes.pdf timer.pdf PieceManager.pdf

%.pdf: %.dot
	dot -Tpdf $<  > $@

clean:
	rm -f *.pdf



