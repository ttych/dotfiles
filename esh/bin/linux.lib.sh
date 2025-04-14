#!/bin/sh
# -*- mode: sh -*-

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`



######################################### guard clause

if [ "$(uname)" != "Linux" ]; then
  echo "This script is for Linux. Aborting."
  return 1 2>/dev/null || exit 1
fi



######################################### entropy

LINUX_PROC_ENTROPY="/proc/sys/kernel/random/entropy_avail"
LINUX_PROC_ENTROPY_BASE=1000

cat_entropy()
{
    if [ -z "$LINUX_PROC_ENTROPY" ] || [ ! -r "$LINUX_PROC_ENTROPY" ]; then
        echo >&2 "no entropy file ($LINUX_PROC_ENTROPY)"
        return 1
    fi

    cat_entropy=`cat "$LINUX_PROC_ENTROPY"`
    echo "$cat_entropy"

    if [ "$cat_entropy" -le "$LINUX_PROC_ENTROPY_BASE" ]; then
        echo >&2 "not enough, < $LINUX_PROC_ENTROPY_BASE"
    fi
}




######################################### main

case "$SCRIPT_NAME" in
    cat_entropy)
        "$SCRIPT_NAME" "$@"
        ;;
esac
