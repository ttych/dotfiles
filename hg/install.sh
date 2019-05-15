#!/bin/sh

echo "[ui]
username = $SENV_NAME <$SENV_MAIL>
verbose = True
editor = vim
merge = internal:merge

[extensions]
# (see \"hg help extensions\" for more info)
# pager =
# progress =
# color =

[alias]

" > $INSTALL_DIR/.hgrc
