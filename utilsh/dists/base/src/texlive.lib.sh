#!/bin/sh
# -*- mode: sh -*-

#########################################
# texlive
#########################################

texlive_env()
{
    _texlive_env__os_arch="$(uname -m)"
    _texlive_env__os_name="$(uname -s | tr '[A-Z]' '[a-z]')"
    DEFAULT_TEXLIVE_DIR="$HOME/local/texlive"
    TEXLIVE_DIR="${TEXLIVE_DIR:-$DEFAULT_TEXLIVE_DIR}"
    export TEXLIVE_DIR
    TEXMFHOME="${TEXMFHOME:-$HOME/.texmf}"
    export TEXMFHOME

    if [ -d "$TEXLIVE_DIR" ]; then
        for _texlive_env__d in $(ls -1d "$TEXLIVE_DIR"/ "$TEXLIVE_DIR"/[0-9]*/ 2>/dev/null | sort -r)
        do
            _texlive_env__d="${_texlive_env__d%/}"
            [ -x "${_texlive_env__d}/${TEXLIVE_INSTALLER}" ] || continue
            [ -d "${_texlive_env__d}/bin" ] || continue
            [ -d "${_texlive_env__d}/bin/${_texlive_env__os_arch}-${_texlive_env__os_name}" ] || continue

            path_add_pre "${_texlive_env__d}/bin/${_texlive_env__os_arch}-${_texlive_env__os_name}"
            MANPATH="${_texlive_env__d}/texmf-dist/doc/man:$MANPATH"
            export MANPATH
            INFOPATH="${_texlive_env__d}/texmf-dist/doc/info:$INFOPATH"
            export INFOPATH
            break
        done
    fi
}

texlive_env

#########################################
