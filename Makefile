all:
	R --no-save --no-restore -e "require(cacheSweave); setCacheDir('./cache'); Sweave('fascicule_r_stata.rnw', driver=cacheSweaveDriver, encoding='utf8')"
#	./hw_crop.sh
	xelatex fascicule_r_stata.tex
	bibtex fascicule_r_stata
	xelatex fascicule_r_stata.tex
#	makeindex full
	xelatex fascicule_r_stata.tex
	rm *.{aux,bbl,blg,run.xml}
