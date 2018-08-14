set -x ASPELL_CONF "data-dir /run/current-system/sw/lib/aspell/"
set -x EDITOR 'emacsclient -cnw -a ""'
# set -x MANPATH $MANPATH /usr/share/man /usr/local/share/man /usr/X11/share/man
# set -x MANPATH /run/current-system/sw/share/man $MANPATH
set -x PATH ~/.local/bin ~/bin /run/current-system/sw/bin $PATH
set -x VISUAL $EDITOR


set fish_greeting
