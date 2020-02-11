#! /usr/bin/env bash

set -euo pipefail

[ -z "$1" ] && {
    echo "No argument supplied"
    exit 1
}

[ -f "$1" ] || {
    echo "$1 not found"
    exit 1
}

type biber >/dev/null 2>&1 || {
    echo "Please install biber"
    exit 1
}

biber \
    --nolog \
    --output-align \
    --output-fieldcase=lower \
    --output-file /dev/stdout \
    --output-resolve \
    --output-safechars \
    --quiet --quiet \
    --tool \
    "$1" |
    head -n-1 |
    sponge "$1"
