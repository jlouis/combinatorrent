.PHONY: all

all: processes.pdf timer.pdf

%.pdf: %.dot
	dot -Tpdf $<  > $@

clean:
	rm -f *.pdf



