#!/bin/sh

# -*- mode: sh -*-

snippets_repo='https://gitlab.com/ttych/emacs-yasnippets.git'

install_snippets()
{
    if [ -d "snippets" ]; then
        rm -Rf "snippets" || return $?
    fi
    git clone "$snippets_repo" "snippets" || return $?
}

install_snippets
