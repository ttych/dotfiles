#!/bin/sh

zpool create \
      -o ashift=12 \
      -O dnodesize=auto \
      -O compression=lz4 \
      -O mountpoint=legacy \
      -O atime=off \
      -O acltype=posixacl \
      -o feature@userobj_accounting=disabled \
      "$@"
