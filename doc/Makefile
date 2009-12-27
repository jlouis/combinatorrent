.PHONY: all

all: processes.pdf timer.pdf PieceManager.pdf Peer.pdf

%.pdf: %.dot
	dot -Tpdf $<  > $@

clean:
	rm -f *.pdf



