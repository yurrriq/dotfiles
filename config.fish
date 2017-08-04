# -*- mode: fish; tab-width: 2; -*-

# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme yurrriq

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

# source ~/src/sjl/z-fish/z.fish

# set -x MONO_GAC_PREFIX "/usr/local"

# set -x GOPATH $HOME/src/go
# set -x PATH ~/bin $PATH $HOME/src/go/bin /usr/local/opt/go/libexec/bin
# set -x PATH ~/bin $PATH $HOME/src/go/bin
set -x PATH ~/bin $PATH

# set -x GHC_DOT_APP ~/Applications/ghc-7.10.2.app
# set -x PATH $HOME/.cabal/bin $HOME/.local/bin {$GHC_DOT_APP}/Contents/bin $PATH
set -x PATH $HOME/.cabal/bin $HOME/.local/bin $PATH

# Add Idris sandbox to PATH
set -x PATH $PATH $HOME/src/idris-lang/idris-dev/.cabal-sandbox/bin

# source ~/src/erlang/18.0/activate.fish

# set LEIN_JAVA_CMD (which drip)
set GPG_TTY (tty)

set -x LEIN_FAST_TRAMPOLINE y

# set -x SLIMERJSLAUNCHER = ~/Applications/Firefox.app/Contents/MacOS/firefox

set -x MANPATH $MANPATH /usr/share/man /usr/local/share/man /usr/X11/share/man
set -x MANPATH /run/current-system/sw/share/man $MANPATH

function cljsbuild
	lein trampoline cljsbuild $argv
end

function l
	ls -lsa $argv
end

function k
	clear
end

function cloc
	perl ~/bin/cloc-1.64.pl $argv
end

function toggle_hidden_files
	fish ~/src/scripts/toggle_hidden_files.fish
end

function thf
	toggle_hidden_files
end

function playmidi
	fluidsynth -i ~/lib/arachno-soundfont/Arachno\ SoundFont\ -\ Version\ 1.0.sf2 $argv
end

function agn
	ag --nogroup $argv
end

function et -d 'Start an Emacs client in the terminal, starting the server as necessary.'
  emacsclient -cnw -a "" $argv
end

function ec -d 'Start an Emacs client in the Cocoa GUI, starting the server as necessary.'
  emacsclient -cna "" $argv
end

function e -d 'Even shorter alias for `ec`'
  ec $argv
end

function magit-status -d 'Start an Emacs client in the terminal,
  call magit-status with the current working directory, and delete other windows.'
  et -e '(progn (magit-status "'(pwd)'") (delete-other-windows))'
end

# set -x EDITOR "emacsclient -cnw -a ''"
# set -x EDITOR "emacsclient -nw -a ''"
set -x EDITOR 'emacsclient -cnw -a "" $argv'
set -x VISUAL $EDITOR

# function nw
# 	/Applications/nwjs.app/Contents/MacOS/nwjs $argv
# end

function __thefuck_repl -d 'Replace operators into fish-compatible'
  set -l tmp (echo $argv | sed 's/ && / ; and /g')
  echo $tmp | sed 's/ || / ; or /g'
end

function fuck -d 'Correct your previous console command'
  set -l eval_script (mktemp 2>/dev/null ; or mktemp -t 'thefuck')
  thefuck $history[1] > $eval_script
  eval (__thefuck_repl (cat $eval_script))
  rm $eval_script
end

function lalily
  lilypond -I lalily -djob-count=8 -dmidi-extension=mid $argv
end

# OPAM configuration
# . /Users/mohacker/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true

# function rethinkdb-start
#   launchctl load /usr/local/opt/rethinkdb/homebrew.mxcl.rethinkdb.plist
# end

# function rethinkdb-stop
#   launchctl unload /usr/local/opt/rethinkdb/homebrew.mxcl.rethinkdb.plist
# end

# function elasticsearch-start
#   launchctl load ~/Library/LaunchAgents/homebrew.mxcl.elasticsearch.plist
# end

# function elasticsearch-stop
#   launchctl unload ~/Library/LaunchAgents/homebrew.mxcl.elasticsearch.plist
# end

source ~/.config/fish/secrets.fish

# set -x PATH $HOME/.cim/bin $PATH
# set -x CIM_HOME $HOME/.cim
# source $CIM_HOME/init.fish; and source $CIM_HOME/init.fsh

function llfe -d "Literate Lisp Flavoured Erlang"
  eval $HOME/src/llfe/llfe/llfe $argv
end

function r3 -d "A short alias for rebar3"
  rebar3 $argv
end

# perlbrew
. ~/perl5/perlbrew/etc/perlbrew.fish

# nix
# if test "$IN_NIX_SHELL" != "1"
#   . ~/.nix-profile/etc/profile.d/nix.fish
# end

set -x CARGO_HOME "$HOME/.cargo"

# rebar3
set -x PATH $PATH ~/.cache/rebar3/bin

# cargo
set -x PATH "$CARGO_HOME/bin" $PATH

# source ~/.iterm2_shell_integration.fish

# source /run/current-system/sw/share/autojump/autojump.fish

# function hicat -d 'A hackish hicat clone via highlight'
#   highlight -O xterm256 $argv | less -cR
# end

set -x PATH $PATH /usr/local/texlive/2017/bin/x86_64-darwin
