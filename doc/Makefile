.PHONY: all

all: processes.pdf timer.pdf PeerManager.pdf

%.pdf: %.dot
	dot -Tpdf $<  > $@

clean:
	rm -f *.pdf



