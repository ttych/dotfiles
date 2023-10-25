#!/bin/sh
# -*- mode: sh -*-

### pkgconfig

pkgconfig_path_pprint()
{
    echo "$PKG_CONFIG_PATH" | tr ':' '\012'
}

pkgconfig_path_clear()
{
    unset PKG_CONFIG_PATH 2>/dev/null || export PKG_CONFIG_PATH=
}

pkgconfig_path_add()
{
    for pkgconfig_add__p; do
        [ -z "$pkgconfig_add__p" ] && continue
        [ -d "$pkgconfig_add__p" ] || continue

        PKG_CONFIG_PATH="${pkgconfig_add__p}${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
    done
    if [ -n "$PKG_CONFIG_PATH" ]; then
        export PKG_CONFIG_PATH
    fi
}
