#!/bin/sh
# -*- mode: sh -*-


SCRIPT_NAME="${0##*/}"


ssh_pub_from_priv()
{
    if [ ! -r "$1" ]; then
        echo >&2 "private key '$1' is not readable."
        return 1
    fi

    if [ -r "$1.pub" ]; then
        echo >&2 "public key '$1.pub' already exists."
        return 1
    fi

    if ! ssh-keygen -y -f "$1" > "$1".pub; then
        rm -f "$1".pub
        return 1
    fi
}



### main

case "$SCRIPT_NAME" in
    ssh_*) "$SCRIPT_NAME" "$@"
           ;;
esac
