source /run/current-system/sw/share/autojump/autojump.fish

function pcat
	pygmentize -f terminal -g $argv
end

function hicat -d 'Hackish hicat clone via pygments'
	pcat $argv | less -cR
end

set -x EDITOR 'et'
set -x VISUAL 'e'

set fish_greeting
