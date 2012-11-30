all:
#	xelatex hw.tex
	R --no-save --no-restore -e "require(cacheSweave); setCacheDir('./cache'); Sweave('full.rnw', driver=cacheSweaveDriver, encoding='utf8')"
#	R CMD Stangle full.rnw
#	pygmentize -O full,style=emacs -l splus -o full.html full.R
#	./hw_crop.sh
	xelatex full.tex
	bibtex full
	xelatex full.tex
	makeindex full
	xelatex full.tex
	rm *.{aux,bbl,blg,log,run.xml}
