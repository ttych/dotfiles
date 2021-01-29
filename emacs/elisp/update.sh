#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


wget -q -O "$SCRIPT_PATH/iy-go-to-char.el" "https://raw.githubusercontent.com/doitian/iy-go-to-char/master/iy-go-to-char.el"

wget -q -O "$SCRIPT_PATH/grep-a-lot.el" "http://www.emacswiki.org/emacs/download/grep-a-lot.el"

wget -q -O "$SCRIPT_PATH/turnip.el" "https://raw.githubusercontent.com/kljohann/turnip.el/master/turnip.el"

wget -q -O "$SCRIPT_PATH/dired-details.el" "http://www.emacswiki.org/emacs/download/dired-details.el"
