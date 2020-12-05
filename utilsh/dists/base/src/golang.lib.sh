#!/bin/sh
# -*- mode: sh -*-

for home in "$HOME" "$HOME_ALT"; do
    if [ -d "$home" ]; then
        GOPATH="$home/local/$(uname -s)/$(uname -m)/gopath${GOPATH:+:$GOPATH}"
    fi
done
export GOPATH
export GOPATH_1="${GOPATH%%:*}"
