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

source ~/src/scripts/z.fish

set -x GOPATH $HOME/src/go
set PATH $PATH ~/src/scripts $HOME/src/go

source ~/src/erlang/17.5/activate.fish

# set LEIN_JAVA_CMD (which drip)
set GPG_TTY (tty)

set -x LEIN_FAST_TRAMPOLINE y

set -x SLIMERJSLAUNCHER = ~/Applications/Firefox.app/Contents/MacOS/firefox

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
	perl ~/src/scripts/cloc-1.62.pl $argv
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

function ec
	emacsclient $argv
end

function nw
	~/Applications/nwjs-v0.12.0-osx-x64/nwjs.app/Contents/MacOS/nwjs $argv
end

function fuck
  eval (thefuck $history[1])
end
