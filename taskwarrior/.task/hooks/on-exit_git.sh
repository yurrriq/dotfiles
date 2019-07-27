#! /usr/bin/env bash

set -eufo pipefail

args="${2#args:}"
command="${3#command:}"
data="${5#data:}"

if git -C "$data" -C "$data" diff --quiet; then
    if [ -n "${DEBUG:-}" ]; then
        echo 'No changes to commit'
    fi
    exit 0
fi

if ! git -C "$data" -C "$data" add --all; then
    echo 'Failed to add files to the index'
    exit 100
fi

if ! git -C "$data" -C "$data" commit --quiet --message="$command: ${args#task $command }"; then
    echo 'Failed to record changes to the repository'
    exit 101
fi

if [ -n "${DEBUG:-}" ]; then
    git -C "$data" -C "$data" log --oneline --max-count=1
fi
