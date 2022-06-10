#!/bin/sh
# -*- mode: sh -*-

#########################################
# texlive
#########################################

texlive_env()
{
    texlive_env__os_arch="$(uname -m)"
    texlive_env__os_name="$(uname -s | tr '[A-Z]' '[a-z]')"
    DEFAULT_TEXLIVE_DIR="$HOME/local/texlive"
    TEXLIVE_DIR="${TEXLIVE_DIR:-$DEFAULT_TEXLIVE_DIR}"
    export TEXLIVE_DIR
    TEXMFHOME="${TEXMFHOME:-$HOME/.texmf}"
    export TEXMFHOME

    if [ -d "$TEXLIVE_DIR" ]; then
        for texlive_env__d in $(ls -1d "$TEXLIVE_DIR"/ "$TEXLIVE_DIR"/[0-9]*/ 2>/dev/null | sort -r)
        do
            [ -x "${texlive_env__d}${TEXLIVE_INSTALLER}" ] || continue
            [ -d "${texlive_env__d}bin" ] || continue
            [ -d "${texlive_env__d}bin/${texlive_env__os_arch}-${texlive_env__os_name}" ] || continue

            path_add_pre "${texlive_env__d}bin/${texlive_env__os_arch}-${texlive_env__os_name}"
            MANPATH="${texlive_env__d}texmf-dist/doc/man:$MANPATH"
            export MANPATH
            INFOPATH="${texlive_env__d}texmf-dist/doc/info:$INFOPATH"
            export INFOPATH
            break
        done
    fi
}

texlive_env

#########################################
