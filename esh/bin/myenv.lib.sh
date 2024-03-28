#!/bin/sh
# -*- mode: sh -*-


MYENV_DIR="${MYENV_DIR:-$HOME/etc/myenv}"

MYENV_LOADED="${MYENV_LOADED:-:}"

MYENV_LOAD_FUNCTION=myenv_load
MYENV_UNLOAD_FUNCTION=myenv_unload

MYENV_LIST_SEP="${MYENV_DISP_SEP:-|}"
MYENV_LIST_FORMAT="%40s $MYENV_LIST_SEP %3s\n"


### lib
__myenv_dir()
{
    mkdir -p "$MYENV_DIR" &&
        chmod 0700 "$MYENV_DIR"
}


### help
__myenv_help()
{
    cat <<EOF
Usage:
    myenv [action] [...]

with action in:
    h | help           : display help
    l | list           : list available env
    s | set | load     : load env
    u | unset | unload : unload env
    r | reload         : reload env
    c | create         : create env

EOF
}


### list
__myenv_list()
(
    __myenv_dir || return 1
    cd "$MYENV_DIR" || return  1

    __myenv_list_print_line name set
    __myenv_list_print_line -------------------- \
                            ---
    for f in *; do
        [ "$f" = "*" ] && continue
        [ -f "$f" ] || continue
        case "$f" in
            _*) continue ;;
        esac
        __myenv_list_env "$f"
    done
)

__myenv_list_print_line()
{
    printf "$MYENV_LIST_FORMAT" "$@"
}

__myenv_list_env()
(
    __myenv_list_env__name="$1"

    if __myenv_loaded "$__myenv_list_env__name"; then
        __myenv_list_env__set=x
    else
        __myenv_list_env__set=
    fi

    __myenv_list_print_line "$__myenv_list_env__name" \
                            "$__myenv_list_env__set"
)


### load
__myenv_load()
{
    __myenv_loaded "$1" && return 1

    [ -f "$MYENV_DIR/$1" ] && [ -r "$MYENV_DIR/$1" ] || return 1

    __myenv_load_clean

    __myenv_load__pwd="$PWD"
    cd "$MYENV_DIR" &&
        . ./"$1" &&
        $MYENV_LOAD_FUNCTION &&
        __myenv_load_clean &&
        MYENV_LOADED="${MYENV_LOADED}${1}:"
    cd "$__myenv_load__pwd"
}

__myenv_loaded()
{
    [ -z "$1" ] && return 1

    case "$MYENV_LOADED" in
        *":$1:"*) return 0 ;;
        *) return 1 ;;
    esac
}

__myenv_load_clean()
{
    unset $MYENV_LOAD_FUNCTION 2>/dev/null >/dev/null
    unset $MYENV_UNLOAD_FUNCTION 2>/dev/null >/dev/null
    return 0
}


### unload
__myenv_unload()
{
    __myenv_loaded "$1" || return 1

    __myenv_load_clean

    __myenv_unload__pwd="$PWD"
    cd "$MYENV_DIR" &&
        . ./"$1" &&
        $MYENV_UNLOAD_FUNCTION && {
            __myenv_unload__pre="${MYENV_LOADED%:$1*}"
            __myenv_unload__post="${MYENV_LOADED#*$1:}"
            MYENV_LOADED="$__myenv_unload__pre:$__myenv_unload__post"
        }
    cd "$__myenv_unload__pwd"
}


### reload
__myenv_reload()
{
    __myenv_unload "$@" &&
        __myenv_reload "$@"
}


### create
__myenv_create()
{
    __myenv_create__name="$1"
    [ -z "$__myenv_create__name" ] && return 1
    [ -r "$MYENV_DIR/$__myenv_create__name" ] && return 1
    shift

    __myenv_create__load_content=
    __myenv_create__unload_content=
    for __myenv_create__v; do
        eval __myenv_create__v_value="\"\$$__myenv_create__v\""

        __myenv_create__load_content="$__myenv_create__load_content
$__myenv_create__v='$__myenv_create__v_value'"
        __myenv_create__unload_content="$__myenv_create__unload_content
unset $__myenv_create__v"
    done

    __myenv_create__load_content="${__myenv_create__load_content:-:}"
    __myenv_create__unload_content="${__myenv_create__unload_content:-:}"

    cat <<EOF > "$MYENV_DIR/$__myenv_create__name"
# -*- mode: sh -*-

myenv_load()
{
$__myenv_create__load_content
}

myenv_unload()
{
$__myenv_create__unload_content
}
EOF
}


### myenv
__myenv()
{
    __myenv=
    case "$1" in
        -h|--help|h|help)
            __myenv_help
            ;;
        ""|l|list)
            __myenv_list
            ;;
        s|"set"|load)
            shift
            __myenv_load "$@"
            ;;
        u|"unset"|unload)
            shift
            __myenv_unload "$@"
            ;;
        r|reset|reload)
            shift
            __myenv_reload "$@"
            ;;
        c|create)
            shift
            __myenv_create "$@"
            ;;
        *)
            __myenv_load "$@"
            ;;
    esac
}

myenv()
{
    __myenv "$@"
    myenv="$?"
    [ -n "$__myenv" ] && printf "%s\n" "$__myenv"
    return $myenv
}
