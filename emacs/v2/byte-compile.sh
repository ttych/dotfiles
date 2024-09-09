#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`

emacs --batch --user=thomas --eval "(byte-recompile-directory (expand-file-name \"$SCRIPT_PATH\") 0)"
