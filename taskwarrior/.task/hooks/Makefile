.PHONY: all
all: on-exit_git.sh on-exit_git.pdf

%.sh: %.nw
	@ notangle $< | cpif $@
	@ chmod a+x $@

%.tex: %.nw
	@ noweave -n -delay -index $< | cpif $@

%.pdf: %.tex
	@ latexmk -bibtex -pdf -gg $<
	@ latexmk -bibtex -pdf -c $<
