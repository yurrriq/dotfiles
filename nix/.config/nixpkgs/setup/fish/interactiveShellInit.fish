type -p pygmentize >/dev/null 2>&1; and function pcat
    pygmentize -f terminal -g $argv
end; and function hicat -d 'Hackish hicat clone via pygments'
    pcat $argv | less -cR
end

type -p fluidsynth >/dev/null 2>&1; and function playmidi
    fluidsynth -i ~/lib/arachno-soundfont/Arachno\ SoundFont\ -\ Version\ 1.0.sf2 $argv
end


type -p kubectl >/dev/null 2>&1; and function kcexec
    argparse -N 2 --name=kcexec 'r/replica=' -- $argv
    or return

    if not set -q _flag_replica
        set -l _flag_replica 0
    end

    kubectl exec -it (kubectl get pod -o jsonpath="{.items[$_flag_replica].metadata.name}" -l app.kubernetes.io/name=$argv[1]) -c $argv[1] $argv[2..-1]
end


# set fish_path $HOME/.oh-my-fish
# set fish_theme yurrriq
# . $fish_path/oh-my-fish.fish


functions rvm >/dev/null 2>&1; and rvm default

set -x PATH ~/bin $PATH

set fish_greeting
