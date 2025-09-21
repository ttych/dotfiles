#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


KEYCHAIN_URL='https://raw.githubusercontent.com/funtoo/keychain/refs/heads/master/keychain'


CURL()
{
    curl -fsSL $CURL_NO_SSL ${2:+-o "$2"} "$1"
}
WGET()
{
    wget -q $WGET_NO_SSL ${2:+-O "$2"} "$1"
}
download()
{
    which curl >/dev/null 2>/dev/null &&
        CURL "$@" &&
        return 0
    which wget >/dev/null 2>/dev/null &&
        WGET "$@" &&
        return 0
    return 1
}

set -x
download "$KEYCHAIN_URL" "$SCRIPT_PATH/keychain"
set +x
