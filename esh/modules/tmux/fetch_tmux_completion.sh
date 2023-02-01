#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`

RAW_URL='https://raw.githubusercontent.com/tmuxinator/tmuxinator/master/completion/'

CURL()
{
    curl -s -S -L $CURL_NO_SSL ${2:+-o "$2"} "$1"
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

for script in tmuxinator.bash tmuxinator.zsh
do
    download "$RAW_URL/$script" "$SCRIPT_PATH/$script"
done
