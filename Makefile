all: ~/.emacs.d/README.org ~/.emacs.d/org-info.js ~/.emacs.d/style.min.css ~/.emacs.d/init.el ~/.emacs.d/themes ~/.emacs.d/lisp
	rsync -avz --exclude emacs-color-theme-solarized $^ .

clean:
	rm -rf init.el lisp themes
