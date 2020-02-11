#! /usr/bin/env bash

set -euo pipefail

shfmt -i 4 -ci -s -w "$@"
