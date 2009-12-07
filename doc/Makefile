.PHONY: all

all: processes.pdf

%.pdf: %.dot
	dot -Tpdf $<  > $@

clean:
	rm -f *.pdf



