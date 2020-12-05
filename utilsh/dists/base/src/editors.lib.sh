#!/bin/sh
# -*- mode: sh -*-

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


### emacs
if [ -n "$ESH_SH_CUR_SHORT" ]; then
    ESHELL="${ESH_SH_CUR_SHORT}_login_shell"
    export ESHELL
fi

if [ -d "$HOME_ALT" ] && [ -d "$HOME_ALT/.emacs.d" ]; then
    EMACS_WORKING_DIR="$HOME_ALT/.emacs.d"
    export EMACS_WORKING_DIR
fi

_emacs_guess()
{
    if [ -z "$_EMACS" ] || [ "$_EMACS" = ":" ]; then
        _EMACS=`command -v emacs`
        [ -z "$_EMACS" ] && _EMACS=`command -v xemacs`
        _EMACS=${_EMACS:-:}
    fi
    if [ -z "$_EMACS_TERM" ]; then
        case "$TERM" in
            xterm-*) _EMACS_TERM="$TERM" ;;
            *-256color) _EMACS_TERM="xterm-256color" ;;
            *) _EMACS_TERM="$TERM" ;;
        esac
    fi
}
_emacs_guess
_emacs()
{
    TERM="$_EMACS_TERM" $_EMACS "$@"
}

_emacs_reset()
{
    _EMACS=
    _EMACSCLIENT=
    _EMACS_TERM=
}

#e() { emacs -fn -\*-fixed-\*-12-\* "$@" & }
emx()
{
    _emacs "$@" &
}

emqx()
{
    emx -quick "$@"
}

emt()
{
    _emacs -nw "$@"
}

emqt()
{
    emt -quick "$@"
}

emc()
{
    emt "$@"
}

emqc()
{
    emqt "$@"
}

_emacsclient_guess()
{
    if [ -z "$_EMACSCLIENT" ] || [ "$_EMACSCLIENT" = ":" ]; then
        _EMACSCLIENT=`command -v emacsclient`
        [ -z "$_EMACSCLIENT" ] && _EMACSCLIENT=`command -v xemacsclient`
         _EMACSCLIENT=${_EMACSCLIENT:-:}
    fi
    if [ -z "$_EMACS_TERM" ]; then
        case "$TERM" in
            xterm-*) _EMACS_TERM="$TERM" ;;
            *-256color) _EMACS_TERM="xterm-256color" ;;
            *) _EMACS_TERM="$TERM" ;;
        esac
    fi
}
_emacsclient_guess

_emacsclient()
{
    TERM="$_EMACS_TERM" $_EMACSCLIENT "$@"
}

# em()
# {
#     ALTERNATE_EDITOR="" TMPDIR="/tmp" _emacsclient --tty "$@"
# }
# emk()
# {
#     em -e '(kill-emacs)'
# }
# emx()
# {
#     em -e '(client-save-kill-emacs)'
# }
# emd()
# {
#     ALTERNATE_EDITOR="" TMPDIR="/tmp" emacs --daemon
# }

ec_c()
{
    TMPDIR="${TMPDIR:-/tmp}" _emacsclient --alternate-editor="$ALTERNATE_EDITOR" "$@"
}
ec_e()
{
    TMPDIR="${TMPDIR:-/tmp}" _emacs --alternate-editor="$ALTERNATE_EDITOR" "$@"
}
ecx()
{
    ec_c --create-frame "$@" &
}
ec()
{
    ec_c --tty "$@"
}
eck()
{
    ec_c -e '(kill-emacs)'
}
ecs()
{
    ec_c --tty -e '(client-save-kill-emacs)'
}
ecd()
{
    ec_e --daemon
}

e()
{
    ec "$@"
}


etodo()
{
    e "$HOME/org/todo.org"
}


### vim

# g reclamed for git !
#g() { gvim "$@" & }


### sublime

sublime()
{
    LANG=${LANG:-en_US.UTF-8} sublime_text "$@"
}


### rubymine

rubymine()
{
    rubymine.sh "$@"
}


### pycharm
pycharm()
{
    pycharm=`command -v pycharm`
    case $pycharm in
        /*) ;;
        *) pycharm= ;;
    esac
    if [ -n "$pycharm" ]; then
        "$pycharm" "$@"
        return $?
    fi

    pycharm=`command -v pycharm.sh`
    if [ -n "$pycharm" ]; then
        "$pycharm" "$@" &
        return $? # always 0
    fi

    echo >&2 'no pycharm distribution found'
    return 1
}


### main

case "$SCRIPT_NAME" in
    e|e?|e??|etodo) "$SCRIPT_NAME" "$@"
             ;;
esac
