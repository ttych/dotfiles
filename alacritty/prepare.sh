#!/bin/sh
# -*- mode: sh -*-

mkdir -p "$INSTALL_DIR/.config/${PWD##*/}"
chmod 0700 "$INSTALL_DIR/.config/${PWD##*/}"
