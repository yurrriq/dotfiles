all: ~/.emacs.d/README.org ~/.emacs.d/init.el ~/.emacs.d/themes ~/.emacs.d/lisp
	rsync -avz --exclude emacs-color-theme-solarized $^ .

clean:
	rm -rf init.el lisp themes
