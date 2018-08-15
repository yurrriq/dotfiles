function pcat
    pygmentize -f terminal -g $argv
end


function hicat -d 'Hackish hicat clone via pygments'
    pcat $argv | less -cR
end


rvm default
