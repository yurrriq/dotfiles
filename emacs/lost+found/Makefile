all: ~/.emacs.d/README.org ~/.emacs.d/org-info.js ~/.emacs.d/style.min.css ~/.emacs.d/init.el ~/.emacs.d/themes ~/.emacs.d/lisp
	rsync -avz --exclude emacs-color-theme-solarized $^ .

install: README.org init.el lisp org-info.js style.min.css
	rsync -avz --exclude Makefile --exclude .git --exclude emacs-color-theme-solarized . ~/.emacs.d

clean:
	rm -rf init.el lisp themes
