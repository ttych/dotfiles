#!/bin/sh
# -*- mode: sh -*-

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


######################################### guard clause
if [ "$(uname)" != "FreeBSD" ]; then
  echo "This script is for FreeBSD. Aborting."
  return 1 2>/dev/null || exit 1
fi



######################################### cpu
cpu_count()
{
    cpu_count=`sysctl -n hw.ncpu`
    echo $cpu_count
}
