#!/bin/sh
# -*- mode: sh -*-


######################################### zpool
zpool_create()
{
    zpool create \
          -o ashift=12 \
          -o autotrim=on \
          -O compression=lz4 \
          -O acltype=posixacl \
          -O xattr=sa \
          -O dnodesize=auto \
          -O normalization=formD \
          -O mountpoint=legacy \
          -O atime=off \
          -O relatime=off \
          -m none \
          "$@"
}


######################################### zfs


######################################### main
case "$SCRIPT_NAME" in
    zpool_*|zfs_*)
        "$SCRIPT_NAME" "$@"
        ;;
esac
