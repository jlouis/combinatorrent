.PHONY: all

all:
	./postproc.hs present stat_db.txt > stat_db.csv
	R --no-save < visualize_stats.R

push:
	cp *.png /home/jlouis/Projects/combinatorrent-gh-pages/img/
	cp *.svg /home/jlouis/Projects/combinatorrent-gh-pages/img/


