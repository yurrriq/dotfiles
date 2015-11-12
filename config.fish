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

source ~/src/sjl/z-fish/z.fish

set -x MONO_GAC_PREFIX "/usr/local"

# set -x GOPATH $HOME/src/go
# set PATH $PATH ~/src/scripts $HOME/src/go

# set -x GHC_DOT_APP ~/Applications/ghc-7.10.2.app
# set -x PATH $HOME/.cabal/bin $HOME/.local/bin {$GHC_DOT_APP}/Contents/bin $PATH
set -x PATH $HOME/.cabal/bin $HOME/.local/bin $PATH

# source ~/src/erlang/18.0/activate.fish

# set LEIN_JAVA_CMD (which drip)
set GPG_TTY (tty)

set -x LEIN_FAST_TRAMPOLINE y

# set -x SLIMERJSLAUNCHER = ~/Applications/Firefox.app/Contents/MacOS/firefox

set -x MANPATH $MANPATH /usr/share/man /usr/local/share/man /usr/X11/share/man

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

function et
  emacsclient -cnw -a "" $argv
end


function ec
	emacsclient -cna "" $argv
end

function e
  ec $argv
end

# set -x EDITOR "emacsclient -cnw -a ''"
# set -x EDITOR "emacsclient -nw -a ''"
set -x EDITOR 'emacsclient -cnw -a "" $argv'
set -x VISUAL $EDITOR

function nw
	~/Applications/nwjs-v0.12.0-osx-x64/nwjs.app/Contents/MacOS/nwjs $argv
end

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

function rethinkdb-start
  launchctl load /usr/local/opt/rethinkdb/homebrew.mxcl.rethinkdb.plist
end

function rethinkdb-stop
  launchctl unload /usr/local/opt/rethinkdb/homebrew.mxcl.rethinkdb.plist
end

function elasticsearch-start
  launchctl load ~/Library/LaunchAgents/homebrew.mxcl.elasticsearch.plist
end

function elasticsearch-stop
  launchctl unload ~/Library/LaunchAgents/homebrew.mxcl.elasticsearch.plist
end

rvm default

eval (docker-machine env default)
