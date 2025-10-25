#!/bin/sh


SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


echo2()
{
    echo >&2 "$@"
}

convert_usage()
{
    cat <<EOF
$SCRIPT_NAME [-h] [-d density] [-q quality] <source> [<target>]
EOF
}

CONVERT_BINARIES="magick convert"
convert__bin=
convert()
{
    if [ -z "$convert__bin" ]; then
        for convert__bin_try in $CONVERT_BINARIES; do
            which "$convert__bin_try" 2>/dev/null >/dev/null \
                && convert__bin="$convert__bin_try" \
                && break
        done
        [ -z "$convert__bin" ] \
            && echo2 "# no binaries found: $CONVERT_BINARIES" \
            && return 1
    fi

    convert__source="$1"
    if [ -z "$convert__source" ]; then
        echo2 "source is empty"
        convert_usage
        return 1
    fi
    convert__target="$2"
    if [ -z "$convert__target" ]; then
        convert__target="${convert__source%.*}.${EXT}"
    fi

    convert__compress="$COMPRESS"
    convert__density="$DENSITY"
    convert__quality="$QUALITY"
    convert__rotate="$ROTATE"

    command \
        $convert__bin \
        -monitor \
        -limit memory 50GiB -limit map 50GiB -limit disk 20GiB \
        "$convert__source" \
        ${convert__compress:+-compress $convert__compress} \
        ${convert__density:+-density $convert__density} \
        ${convert__quality:+-quality $convert__quality} \
        ${convert__rotate:+-rotate $convert__rotate} \
        -strip \
        -sampling-factor 1x1 \
        "$convert__target"

    # -units PixelsPerInch
}


COMPRESS=
#COMPRESS=Lossless
DENSITY=
QUALITY=95
EXT=jpg
ROTATE=

OPTIND=1
while getopts :hc:d:q:r:x opt; do
    case $opt in
        h) ;;
        c) COMPRESS="$OPTARG" ;;
        d) DENSITY="$OPTARG" ;;
        q) QUALITY="$OPTARG" ;;
        r) ROTATE="$OPTARG" ;;
        x) set -x ;;
    esac
done
shift $(($OPTIND - 1))

convert "$@"
