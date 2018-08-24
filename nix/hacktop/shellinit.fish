set fish_path $HOME/.oh-my-fish
set fish_theme yurrriq
. $fish_path/oh-my-fish.fish

source ~/.config/fish/secrets.fish

eval (/run/current-system/sw//bin/direnv hook fish)

source /run/current-system/sw/share/autojump/autojump.fish

set -x MANPATH $MANPATH /usr/share/man /usr/local/share/man /usr/X11/share/man
set -x MANPATH /run/current-system/sw/share/man $MANPATH

function pcat
    pygmentize -f terminal -g $argv
end

function hicat -d 'Hackish hicat clone via pygments'
    pcat $argv | less -cR
end

function playmidi
    fluidsynth -i ~/lib/arachno-soundfont/Arachno\ SoundFont\ -\ Version\ 1.0.sf2 $argv
end

set -x EDITOR 'emacsclient -cnw -a ""'
set -x PATH ~/bin $PATH /usr/local/texlive/2017/bin/x86_64-darwin
set -x VISUAL $EDITOR
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
set fish_greeting
