#!/bin/sh
# -*- mode: sh -*-

echo2()
{
    echo >&2 "$@"
}


GOTO_CONF="${GOTO_CONF:-$HOME/.gotos}"
GOTO_SEP=:

[ ! -f "$GOTO_CONF" ] && touch "$GOTO_CONF"

GOTO_HELP=\
'usage: goto [-h] [action] [tag] [path]

action:
    <tag>          : go to directory associated with tag
    l[ist]         : list all tags
    s[et] tag path : save current or specific path to tag
    u[nset] tag    : delete tag
'

_goto_help()
{
    printf "%s\n" "$GOTO_HELP"
}

_goto_resolve_path()
{
    if [ -d "$1" ]; then
        (cd "$1" && pwd)
    else
        echo "$1"
    fi
}

_goto_list()
{
    while IFS=$GOTO_SEP read -r tag directory; do
        printf '%s -> %s\n' "$tag" "$directory"
    done < "$GOTO_CONF"
}

_goto_get_tags() {
    cut -d: -f1 "$GOTO_CONF" 2>/dev/null
}

_goto_set()
{
    [ -z "$1" ] && { echo2 "Error: Tag name required"; return 1; }

    _goto_set__tag="$1"
    _goto_set__target="$2"

    if [ -z "$_goto_set__target" ] || [ "." = "$_goto_set__target" ]; then
        _goto_set__target="$PWD"
    else
        _goto_set__target=$(_goto_resolve_path "$_goto_set__target")
    fi

    (echo "$_goto_set__tag:$_goto_set__target" ; grep -v "^$_goto_set__tag$GOTO_SEP" "$GOTO_CONF" ; :) > "$GOTO_CONF.$$" &&
        mv "$GOTO_CONF.$$" "$GOTO_CONF"

    # echo "Saved: $_goto_set__tag -> $_goto_set__target"
}

_goto_unset()
{
    [ -z "$1" ] && return 1

    grep -v "^$1$GOTO_SEP" "$GOTO_CONF" > "$GOTO_CONF.$$" &&
        mv "$GOTO_CONF.$$" "$GOTO_CONF"

    # echo "Unset: $1"
}

_goto_to()
{
    [ -z "$1" ] && return 1

    while IFS=$GOTO_SEP read -r tag directory; do
        if [ "$tag" = "$1" ]; then
            _goto_cd "$directory" && _goto_callback
            return $?
        fi
    done < "$GOTO_CONF"

    echo2 "Error: Tag '$1' not found."
    return 1
}

_goto_cd()
{
    cd "$1"
}

GOTO_CALLBACK="${GOTO_CALLBACK:-}"

_goto_callback()
{
    for _goto_callback__c in $GOTO_CALLBACK; do
        if command -v "_goto_callback_$_goto_callback__c" >/dev/null 2>&1; then
            "_goto_callback_$_goto_callback__c"
        fi
    done
}

_goto_callback_add()
{
    [ -z "$1" ] && return 1

    GOTO_CALLBACK="${GOTO_CALLBACK} $1"
}

_goto_dispatch()
{
    OPTIND=1
    while getopts :hx opt; do
        case $opt in
            h) _goto_help; return 0 ;;
            x) set -x ;;
            *) ;;
        esac
    done
    shift $(($OPTIND - 1))

    if [ "$#" -eq 0 ]; then
        _goto_help
        return 0
    fi

    case $1 in
        l|list)
            shift
            _goto_list
            ;;
        s|set)
            shift
            _goto_set "$@"
            ;;
        u|unset)
            shift
            _goto_unset "$@"
            ;;
        help)
            _goto_help
            ;;
        *)
            _goto_to "$1"
            ;;
    esac
    set +x
}

goto()
{
    _goto_dispatch "$@"
}

if ! command -v "gt" >/dev/null 2>&1; then
    gt()
    {
        goto "$@"
    }
fi


if [ -n "$BASH_VERSION" ]; then
    eval '
    _goto_complete_bash() {
        local cur prev
        cur="${COMP_WORDS[COMP_CWORD]}"
        prev="${COMP_WORDS[COMP_CWORD-1]}"
        if [[ "$prev" == "s" || "$prev" == "set" ]]; then
            COMPREPLY=( $(compgen -d -- "$cur") )
        else
            COMPREPLY=( $(compgen -W "list set unset $(_goto_get_tags)" -- "$cur") )
        fi
    }
    complete -F _goto_complete_bash goto gt'
elif [ -n "$ZSH_VERSION" ]; then
    eval '
    _goto_complete_zsh() {
        local tags=( $(_goto_get_tags) )
        _arguments "1:action:(list set unset $tags)" "2:tag:($tags)" "3:path:_files -/"
    }
    compdef _goto_complete_zsh goto gt'
fi
