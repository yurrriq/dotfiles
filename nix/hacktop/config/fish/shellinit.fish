source ~/.config/fish/secrets.fish


set -x EDITOR 'emacsclient -nw -a ""'
set -x MANPATH $MANPATH /usr/share/man /usr/local/share/man /usr/X11/share/man
set -x MANPATH /run/current-system/sw/share/man $MANPATH
set -x PATH ~/bin $PATH /usr/local/texlive/2017/bin/x86_64-darwin $PATH
set -x VISUAL $EDITOR
