#!/bin/sh

echo2()
{
    echo >&2 "$@"
}

usage()
{
    cat >&2 <<EOF
$0 <iso> <device>
EOF
}

dd_iso()
{
    if [ $# -ne 2 ]; then
        usage
        return 1
    fi

    dd_iso__iso="$1"
    dd_iso__device="$2"

    lsblk "$dd_iso__device" > /dev/null || {
        echo2 "#> verify device $dd_iso__device"
        return 1
    }

    dd if="$dd_iso__iso" of="$dd_iso__device" bs=4M status=progress oflag=sync || {
        echo2 "#> failed to write $dd_iso__iso on $dd_iso__device"
        return 1
    }

    eject "$dd_iso__device"
}


dd_iso "$@"
