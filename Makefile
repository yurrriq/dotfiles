%.tex: %.nw
	noweave -filter noweb-minted -delay -latex $< >$@

%.pdf: %.tex
	pdflatex --shell-escape $<
