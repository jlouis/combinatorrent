.PHONY: all

all: processes.pdf timer.pdf PieceManager.pdf Peer.pdf piecemanager.html PHierachy.pdf

%.pdf: %.dot
	dot -Tpdf $<  > $@

%.html: %.lhs
	pandoc --smart --from=markdown+lhs --to=html+lhs $< > $@

clean:
	rm -f *.pdf



