# functions and aliases for using org-doing from a BASH shell
# Copyright (C) 2014 Rudolf Olah <omouse@gmail.com>
# Licensed under the GNU GPL version 3 or later

# Requires ORG_DOING_PATH to be defined
org_doing () {
    emacs --batch -Q -l "$ORG_DOING_PATH" -f 'org-doing' <<< "$@" 1>/dev/null 2>/dev/null
}

alias doing=org_doing
