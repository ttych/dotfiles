#!/bin/sh

WUCP_DIR="${WUCP_DIR:-$HOME/etc/ucp}"
WUCP_SEP="${WUCP_SEP:-|}"
WUCP_BASE_VARS="DOCKER_TLS_VERIFY
                DOCKER_CERT_PATH
                DOCKER_HOST"
WUCP_VARS="COMPOSE_TLS_VERSION
           $WUCP_BASE_VARS
           WUCP_LOADED_CRED_CK"
WUCP_LIST_FORMAT="%15s $WUCP_SEP %6s $WUCP_SEP %50s $WUCP_SEP %3s\n"


### common

_wucp_is_bundle()
{
    [ -z "$1" ] && return 1
    [ -d "$WUCP_DIR/$1" ] || return 1
    [ -r "$WUCP_DIR/$1/env.sh" ] || return 1
    return 0
}

_wucp_ucp_loaded()
{
    for _wucp_ucp_loaded__v in $WUCP_BASE_VARS; do
        eval _wucp_ucp_loaded__c=\"\$$_wucp_ucp_loaded__v\"
        if [ -z "$_wucp_ucp_loaded__c" ]; then
            return 1
        fi
    done

    if [ -n "$1" ]; then
        _wucp_ucp_loaded__bkp_host="$DOCKER_HOST"
        _wucp_ucp_loaded__bkp_cred_ck="$WUCP_LOADED_CRED_CK"
        (
            _wucp_unload &&
                _wucp_load "$1" &&
                [ "$_wucp_ucp_loaded__bkp_host" = "$DOCKER_HOST" ] &&
                [ "$_wucp_ucp_loaded__bkp_cred_ck" = "$WUCP_LOADED_CRED_CK" ]
        ) || return 1
    fi
}

_wucp_compute_cred_ck()
{
    _wucp_compute_cred_ck=
    _wucp_compute_cred_ck__path=.
    if [ -n "$1" ]; then
        _wucp_compute_cred_ck__path="$WUCP_DIR/$1"
    fi
    if [ -r "$_wucp_compute_cred_ck__path/cert.pem" ] &&
           [ -r "$_wucp_compute_cred_ck__path/key.pem" ]; then
        _wucp_compute_cred_ck=$(
            cat "$_wucp_compute_cred_ck__path/cert.pem" "$_wucp_compute_cred_ck__path/key.pem" |
                md5sum -)
        _wucp_compute_cred_ck="${_wucp_compute_cred_ck%% *}"
        return 0
    fi
    return 1
}


### list

_wucp_list()
(
    cd "$WUCP_DIR" || return 0

    _wucp_print_ucp_line name valid host cur
    _wucp_print_ucp_line --------------- \
                         ------ \
                         -------------------------------------------------- \
                         ---
    for d in */; do
        _wucp_print_ucp "$d"
    done
)

_wucp_print_ucp_line()
{
    printf "$WUCP_LIST_FORMAT" "$@"
}

_wucp_print_ucp()
(
    [ -d "$1" ] || return 1
    _wucp_print_ucp__name="${1%/}"
    _wucp_print_ucp__name="${_wucp_print_ucp__name##*/}"
    _wucp_print_ucp__valid=NO
    _wucp_print_ucp__host=
    _wucp_print_ucp__current=

    if _wucp_is_bundle "$1"; then
        if _wucp_ucp_loaded "$1"; then
            _wucp_print_ucp__current='*'
            _wucp_print_ucp__valid=OK
        else
            _wucp_load "$1" &&
                _wucp_print_ucp__valid=OK
        fi
        _wucp_print_ucp__host="$DOCKER_HOST"
    fi

    _wucp_print_ucp_line \
        "$_wucp_print_ucp__name" \
        "$_wucp_print_ucp__valid" \
        "$_wucp_print_ucp__host" \
        "$_wucp_print_ucp__current"
)

### info

_wucp_info()
{
    for _wucp_info__v in $WUCP_VARS; do
        eval _wucp_info__c=\"\$$_wucp_info__v\"
        if [ -n "$_wucp_info__c" ]; then
            echo "export $_wucp_info__v=$_wucp_info__c"
        fi
    done
}


### load

_wucp_load()
{
    _wucp_is_bundle "$1" || return 1

    if _wucp_ucp_loaded; then
        _wucp_unload
    fi

    _wucp_load__pwd="$PWD"

    if cd "$WUCP_DIR/$1" 2>/dev/null; then
        . ./env.sh > /dev/null
        _wucp_compute_cred_ck
        WUCP_LOADED_CRED_CK="$_wucp_compute_cred_ck"
    fi

    cd "$_wucp_load__pwd"
    _wucp_ucp_loaded
}


### unload

_wucp_unload()
{
    for _wucp_unload__v in $WUCP_VARS; do
        eval $_wucp_unload__v=
    done
}


### add

_wucp_add()
(
    [ $# -ne 2 ] && return 1
    [ -z "$1" ] && return 1
    [ -r "$2" ] || return 2

    if [ -r "$WUCP_DIR/$1" ]; then
        echo >&2 "bundle \"$1\" already exists"
        return 1
    fi

    case "$2" in
        /*) _wucp_add__bundle="$2" ;;
        *) _wucp_add__bundle="$PWD/$2" ;;
    esac

    mkdir -p "$WUCP_DIR/$1" &&
        cd "$WUCP_DIR/$1" &&
        unzip -q "$_wucp_add__bundle"
)


### delete

_wucp_delete()
{
    _wucp_is_bundle "$1" || return 1

    if _wucp_ucp_loaded "$1"; then
        _wucp_unload
    fi

    rm -Rf "$WUCP_DIR/$1"
}


### help

_wucp_help()
{
    cat <<EOF
Usage:
    wucp [action] [...]

with action in:
    h | help           : display help
    l | list           : list available ucp bundle
    s | set | load     : load bundle
    u | unset | unload : unload bundle
    a | add            : add bundle
    d | delete         : delete bundle
EOF
}


### main

_wucp()
{
    _wucp=
    case "$1" in
        h|help)
            _wucp_help
            ;;
        l|list)
            _wucp_list
            ;;
        s|"set"|load)
            shift
            _wucp_load "$@"
            ;;
        u|"unset"|unload)
            shift
            _wucp_unload "$@"
            ;;
        a|add)
            shift
            _wucp_add "$@"
            ;;
        d|delete)
            shift
            _wucp_delete "$@"
            ;;
        ""|i|info)
            _wucp_info
            ;;
        *)
            _wucp_load "$@"
            ;;
    esac
}

wucp()
{
    _wucp "$@"
    wucp="$?"
    [ -n "$_wucp" ] && printf "%s\n" "$_wucp"
    return $wucp
}


# if [ "${0##*/}" = "wucp.shl" ] && [ $# -gt 0 ]; then
#      cat >&2 <<EOF
# # not supposed to be used this way
# # first do :
# . $0
# EOF
# fi
