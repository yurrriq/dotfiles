README.org: emacs/README.org fish/README.org
	echo "* My Dotfiles" > README.org
	cat emacs/README.org >> README.org
	cat fish/README.org >> README.org
