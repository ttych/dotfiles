#!/bin/sh
# -*- mode: sh -*-

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


if which code >/dev/null 2>/dev/null; then
    # for ext in $(cat "${SCRIPT_PATH}/extensions.txt"); do
    while read ext; do
        case "$ext" in
            \#*) continue ;;
        esac
        code --install-extension "$ext" # >/dev/null
    done < "${SCRIPT_PATH}/extensions.txt"
else
    echo >&2 "# install vscode conf: no code binary found"
fi
