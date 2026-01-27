#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`



__repeat()
{
    __repeat__delay=
    __repeat__count=
    OPTIND=1
    while getopts :d:c: opt; do
        case $opt in
            d) __repeat__delay="$OPTARG" ;;
            c) __repeat__count="$OPTARG" ;;
            *) echo >&1 "unknown option \"$opt\" for __repeat"
               return 1 ;;
        esac
    done
    shift $(($OPTIND - 1))

    if [ -z "$__repeat__delay" ] && [ -z "$__repeat__count" ]; then
        "$@"
        return $?
    fi

    while : ; do
        "$@"
        __repeat__last_status="$?"

        if [ "$__repeat__count" != "" ]; then
            __repeat__count=$(($__repeat__count - 1))
            if  [ $__repeat__count -le 0 ]; then
                return $__repeat__last_status
            fi
        fi
        sleep ${__repeat__delay:-2}
    done
}



is_wayland() {
    [ -n "$WAYLAND_DISPLAY" ] || [ "$XDG_SESSION_TYPE" = "wayland" ]
}

is_x11()
{
    [ "$XDG_SESSION_TYPE" = "x11" ]
}



has_xclip()
{
    which xclip >/dev/null 2>/dev/null
}

xclip_clipboard_to_primary()
{
    xclip -selection clipboard -o | xclip -selection primary
}

xclip_primary_to_clipboard()
{
    xclip -selection primary -o | xclip -selection clipboard
}



has_xsel()
{
    which xsel >/dev/null 2>/dev/null
}

xsel_clipboard_to_primary()
{
    xsel --clipboard --output | xsel --primary --input
}

xsel_primary_to_clipboard()
{
    xsel --primary --output | xsel --clipboard --input
}



has_wl_clipboard()
{
    which wl-copy >/dev/null 2>/dev/null &&
        which wl-paste >/dev/null 2>/dev/null
}

wl_clipboard_to_primary()
{
    wl-paste | wl-copy --primary
}

wl_primary_to_clipboard()
{
    wl-paste --primary | wl-copy
}



clipboard_to_primary()
{
    if is_x11; then
        if has_xclip; then
            xclip_clipboard_to_primary
            return $?
        fi

        if has_xsel; then
            xsel_clipboard_to_primary
            return $?
        fi
    fi

    if is_wayland; then
        if has_wl_clipboard; then
            wl_clipboard_to_primary
            return $?
        fi
    fi

    return 1
}

primary_to_clipboard()
{
    if is_x11; then
        if has_xclip; then
            xclip_primary_to_clipboard
            return $?
        fi

        if has_xsel; then
            xsel_primary_to_clipboard
            return $?
        fi
    fi

    if is_wayland; then
        if has_wl_clipboard; then
            wl_primary_to_clipboard
            return $?
        fi
    fi

    return 1
}



clip2prim()
{
    __repeat -d "$1" -c "$2" -- clipboard_to_primary
}

prim2clip()
{
    __repeat -d "$1" -c "$2" -- primary_to_clipboard
}



case "$SCRIPT_NAME" in
    clip2prim|prim2clip)
        "$SCRIPT_NAME" "$@"
        exit $?
        ;;
    *)
        echo >&2 "no expected to be called through $SCRIPT_NAME"
        echo >&2 "expected clip2prim or prim2clip"
        exit 1
        ;;
esac
