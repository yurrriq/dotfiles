function pcat
    pygmentize -f terminal -g $argv
end


function hicat -d 'Hackish hicat clone via pygments'
    pcat $argv | less -cR
end


function playmidi
    fluidsynth -i ~/lib/arachno-soundfont/Arachno\ SoundFont\ -\ Version\ 1.0.sf2 $argv
end


# set fish_path $HOME/.oh-my-fish
# set fish_theme yurrriq
# . $fish_path/oh-my-fish.fish


set fish_greeting
