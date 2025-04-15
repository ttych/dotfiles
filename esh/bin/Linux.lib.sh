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



echo2()
{
    echo >&2 "$@"
}



######################################### dpkg
has_dpkg()
{
    which dpkg >/dev/null 2>/dev/null
}

dpkg_latest_kernel_version()
{
    dpkg_latest_kernel_version=$(dpkg-query -W -f='${Version}\n' linux-image-generic)
    dpkg_latest_kernel_version="${dpkg_latest_kernel_version%.*}-generic"
    echo $dpkg_latest_kernel_version
}


######################################### dnf
has_dnf()
{
    which dnf >/dev/null 2>/dev/null
}

dnf_latest_kernel_version()
{
    dnf repoquery --installonly --latest-limit=1 --qf '%{EVR}.${ARCH}'
}



######################################### package
pkg_select()
{
    pkg_select=
    if has_dpkg; then
        pkg_select="dpkg"
    elif has_dnf; then
        pkg_select="dnf"
    fi
    [ -n "$pkg_select" ] && return 0

    echo2 "# no package manager found !"
    return 1
}

pkg_latest_kernel_version()
{
    pkg_select || return 1

    "${pkg_select}_latest_kernel_version"
}



######################################### cpu
cpu_count()
{
    # cpu_count=`grep -c '^processor' /proc/cpuinfo`
    cpu_count=`nproc`
    echo $cpu_count
}



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



######################################### kernel
KERNEL_VERSION=`uname -r`

check_kernel_version()
{
    [ "$(pkg_latest_kernel_version)" = "$KERNEL_VERSION" ] && return 0

    echo2 "# running:$KERNEL_VERSION  installed:$(pkg_latest_kernel_version)"
    return 1
}




######################################### main
case "$SCRIPT_NAME" in
    cat_entropy)
        "$SCRIPT_NAME" "$@"
        ;;
    check_*)
        "$SCRIPT_NAME" "$@"
        ;;
esac
