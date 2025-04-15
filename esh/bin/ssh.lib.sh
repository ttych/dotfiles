#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`



echo2()
{
    echo >&2 "$@"
}



SSH_DEFAULTS="-o StrictHostKeyChecking=no -o CheckHostIP=no -o ConnectTimeout=10"



######################################### key
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



######################################### tunnel
sshtun()
{
    ssh \
        $SSH_DEFAULTS \
        -N \
        -f \
        -q \
        -o ExitOnForwardFailure=yes \
        "$@"
}



######################################### main
case "$SCRIPT_NAME" in
    ssh*)
        "$SCRIPT_NAME" "$@"
        ;;
esac
