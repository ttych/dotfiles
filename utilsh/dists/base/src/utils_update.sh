#!/bin/sh

SCRIPT_PATH="$(cd ${0%/*}; pwd -P)"
UTILS_PATH="${SCRIPT_PATH%/bin}"

cd "$UTILS_PATH" &&
    git pull --no-rebase --ff-only &&
    make
