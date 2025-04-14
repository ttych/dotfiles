#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`



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



clipboard_to_primary()
{
    if has_xclip; then
        xclip_clipboard_to_primary
        return $?
    fi

    if has_xsel; then
        xsel_clipboard_to_primary
        return $?
    fi

    return 1
}

primary_to_clipboard()
{
    if has_xclip; then
        xclip_primary_to_clipboard
        return $?
    fi

    if has_xsel; then
        xsel_primary_to_clipboard
        return $?
    fi

    return 1
}



clip2prim()
{
    while : ; do
        clipboard_to_primary
        [ "$1" != "sync" ] && break
        sleep 1
    done
}

prim2clip()
{
    while : ; do
        primary_to_clipboard "$@"
        [ "$1" != "sync" ] && break
        sleep 1
    done
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
