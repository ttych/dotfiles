#!/bin/sh

# -*- mode: sh -*-

snippets_repo='https://github.com/ttych/emacs-yasnippets.git'

install_snippets()
{
    if [ -d "snippets" ]; then
        rm -Rf "snippets" || return $?
    fi
    git clone "$snippets_repo" "snippets" || return $?
}

install_snippets

#########################################

conf_org='emacs-conf.org'

if ! which org-babel-tangle >/dev/null 2>/dev/null; then
    org-babel-tangle()
    {
        if [ -z "$1" ]; then
            echo >&2 "usage: org-babel-tangle <org-file>"
            return 1
        fi
        emacs --batch "$1" --funcall org-babel-tangle
    }
fi

if [ -r "$conf_org" ]; then
    org-babel-tangle "$conf_org"
fi
