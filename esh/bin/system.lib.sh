#!/bin/sh
# -*- mode: sh -*-


######################################### getent
gh()
{
    getent hosts "$@"
}

gp()
{
    getent passwd "$@"
}



######################################### kill
pstop()
{
    kill -STOP "$@"
}

prun()
{
    kill -CONT "$@"
}



######################################### pkgconfig
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



######################################### ps

psa()
{
    ps -ef "$@"
}

psac()
{
    ps -ef --sort=-%mem "$@"
}

psam()
{
    ps -ef --sort=-%cpu "$@"
}

psat()
{
    ps -ef --sort=-time "$@"
}

psj()
{
    ps -efj "$@"
}

ps_full()
{
    ps -eF "$@"
}

ps_cpu()
{
    ps -ef --sort=-%mem "$@"
}

ps_mem()
{
    ps -ef --sort=-%cpu "$@"
}

ps_time()
{
    ps -edf --sort=-time "$@"
}

ps_thread()
{
    ps -eLf "$@"
}

ps_forest()
{
    ps -efj --forest "$@"
}

ps_sessionid()
{
    ps -o sid= -p "${1:-$$}"
}

# ps_forest()
# {
#     ps --forest -o pid,tty,stat,time,cmd -T -g $(ps_sessionid "$1")
# }
