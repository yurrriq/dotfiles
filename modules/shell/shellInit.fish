source /run/current-system/sw/share/autojump/autojump.fish

function pcat
	pygmentize -f terminal -g $argv
end

function hicat -d 'Hackish hicat clone via pygments'
	pcat $argv | less -cR
end

set -x EDITOR 'emacsclient -cta ""';
set -x VISUAL 'emacsclient -cna ""';

set fish_greeting
