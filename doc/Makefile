.PHONY: all

all: processes.pdf timer.pdf PieceManager.pdf Peer.pdf PHierachy.pdf \
	piecemanager.html on-hieraches.html process-transformers.html \
	processes.png PHierachy.png

%.pdf: %.dot
	dot -Tpdf $<  > $@

%.png: %.dot
	dot -Tpng $< > $@

%.html: %.lhs
	pandoc --smart --from=markdown+lhs --to=html+lhs $< > $@

%.html: %.md
	pandoc --smart --from=markdown --to=html $< > $@

clean:
	rm -f *.pdf



