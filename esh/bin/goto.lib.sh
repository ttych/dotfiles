#!/bin/sh
# -*- mode: sh -*-

GOTO_CONF="$HOME/.gotos"

GOTO_SEP=:

GOTO_HELP=\
'usage: goto [-h] [action] [tag] [path]

action:
    tag            : goto tag
    l[ist]         : list tag info
    s[et] tag path : associate tag to path
    u[nset] tag    : dissociate tag from path
'

_goto_help()
{
    printf "%s\n" "$GOTO_HELP"
}


_goto_list()
{

    while IFS=$GOTO_SEP read tag directory; do
        printf '%s -> %s\n' "$tag" "$directory"
    done < $GOTO_CONF
}

_goto_set()
{
    [ -z "$1" ] && return 1

    _goto_set__tag="$1"
    _goto_set__dir="$2"

    if [ '.' = "$_goto_set__dir" ] || [ '' = "$_goto_set__dir" ]; then
        _goto_set__dir="$PWD"
    fi

    (echo "$_goto_set__tag:$_goto_set__dir" ; grep -v "^$_goto_set__tag$GOTO_SEP" "$GOTO_CONF" ; :) > "$GOTO_CONF.$$" &&
        mv "$GOTO_CONF.$$" "$GOTO_CONF"
}

_goto_unset()
{
    [ -z "$1" ] && return 1

    (grep -v "^$1$GOTO_SEP" "$GOTO_CONF" ; :) > "$GOTO_CONF.$$" && \
        mv "$GOTO_CONF.$$" "$GOTO_CONF"
}

_goto_to()
{
    [ -z "$1" ] && return 1

    while IFS=$GOTO_SEP read tag directory; do
        if [ "$tag" = "$1" ]; then
            _goto_cd "$directory" && _goto_callback
            return $?
        fi
    done < "$GOTO_CONF"
    return 1
}

_goto_cd()
{
    [ -z "$1" ] && return 1

    eval cd \""$1"\"
}

GOTO_CALLBACK="${GOTO_CALLBACK:-}"

_goto_callback()
{
    for _goto_callback__c in $GOTO_CALLBACK; do
        _goto_callback_$_goto_callback__c
    done
}

_goto_callback_add()
{
    [ -z "$1" ] && return 1

    GOTO_CALLBACK="${GOTO_CALLBACK} $1"
}

_goto()
{
    _goto=
    _goto__ret=0
    OPTIND=1
    while getopts :h opt; do
        case $opt in
            h) _goto__act=help ;;
        esac
    done
    shift $(($OPTIND - 1))

    if [ "$#" -eq 0 ] || [ "$_goto__act" = 'help' ] || [ "$1" = 'help' ]; then
        _goto_help
        return 0
    fi

    case $1 in
        l|list) shift
                _goto_list "@"
                _goto__ret=$?
                _goto="$_goto_list" ;;
        s|'set') shift
                 _goto_set "$@"
                 _goto__ret=$?
                 _goto="$_goto_set" ;;
        u|'unset') shift
                   _goto_unset "$@"
                   _goto__ret=$?
                   _goto="$_goto_unset" ;;
        *) _goto_to "$1"
           _goto__ret=$?
           _goto="$_goto_to" ;;
    esac

    return $_goto__ret
}

goto()
{
    _goto "$@"
    goto=$?
    [ -n "$_goto" ] && printf "%s\n" "$_goto"
    return $goto
}

if ! command -v "gt" >/dev/null 2>/dev/null; then
gt()
{
    goto "$@"
}
fi
