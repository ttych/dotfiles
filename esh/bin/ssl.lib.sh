#!/bin/sh
# -*- mode: sh -*-


SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


######################################### documentation

# https://web.archive.org/web/20170107050552/http://shib.kuleuven.be/docs/ssl_commands.shtml



#########################################



######################################### main
case "$SCRIPT_NAME" in
    ssl_*|cert_*|digest_*)
        "$SCRIPT_NAME" "$@"
        ;;
esac
