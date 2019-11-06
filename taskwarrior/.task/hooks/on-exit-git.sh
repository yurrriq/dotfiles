#! /usr/bin/env bash

set -eufo pipefail
if [ -n "${DEBUG:-}" ]; then
    set -x
fi
args="${2#args:}"
command="${3#command:}"
data="${5#data:}"

if git -C "$data" diff --quiet; then
    if [ -n "${DEBUG:-}" ]; then
        echo 'No changes to commit'
    fi
    exit 0
elif ! git -C "$data" add -A; then
    echo 'Failed to add files to the index'
    exit 100
elif ! git -C "$data" commit -qm "$command: ${args#task $command}"; then
    echo 'Failed to record changes to the repository'
    exit 101
elif [ -n "${DEBUG:-}" ]; then
    git -C "$data" log --oneline -1
fi
