all: $(addprefix ${HOME}/.git,\
config \
ignore_global \
message \
)


.git*: README.org
	@ emacs --batch --quick -l ob-tangle --eval '(org-babel-tangle-file "$<")'


${HOME}/%: %
	@ cp -vf $< "$@"
