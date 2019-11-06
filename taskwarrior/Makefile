cpif ?= | cpif

NW_SRCS := \
src/bash/on-exit-git.nw

BASH_SRCS := $(patsubst src/bash/%.nw,.task/hooks/%.sh,$(filter src/bash/%.nw,${NW_SRCS}))

PDF_SRCS := $(patsubst .task/hooks/%.sh,src/tex/%.pdf,${BASH_SRCS})

ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags = -gg
endif

latexmk_flags += -cd -pdf


.PHONY: all
all: ${BASH_SRCS} $(patsubst src/tex/%.pdf,docs/%.pdf,${PDF_SRCS})


.PHONY: install
install: $(patsubst docs/%,src/tex/%,${PDF_SRCS})
	@ mkdir -p $$out
	@ install -m644 -o $$(id -u) -g $$(id -g) -Dt $$out $^


.INTERMEDIATE: ${PDF_SRCS}
docs/%.pdf: src/tex/%.pdf
	@ cp -v $< $@
	@ latexmk $(latexmk_flags) -C $<


src/tex/%.pdf: src/tex/%.tex src/tex/preamble.tex src/tex/postamble.tex
	latexmk $(latexmk_flags) $<


.task/hooks/%.sh: src/bash/%.nw
	notangle $< ${cpif} $@
	@ shellcheck $@
	@ shfmt -d -i 4 $@
	@ chmod a+x $@


src/tex/%.tex: src/bash/%.nw
	noweave -n -delay -index $< ${cpif} $@
