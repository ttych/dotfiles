#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


### mount

mount_devfs()
{
    if df /dev | tail -1 | grep -q '^devfs'; then
        :
    else
        mount -t devfs devfs /dev || return 1
    fi
}


### gpart

gpart_wipes()
{
    for gpart_wipes__d; do
        gpart_wipe "$gpart_wipes__d" || return 1
    done
}

gpart_wipe()
{
    [ -c "/dev/$1" ] || {
        echo >&2 "/dev/$1 is not a character special file"
        return 1
    }

    gpart show "$1" 2>/dev/null >/dev/null || return 0

    gpart_wipe__cmd="gpart destroy -F $1"

    echo >&2 "execute: \"$gpart_wipe__cmd\" ? (y/N)"
    read answer
    case $answer in
        [Yy]|[Yy][Ee][Ss])
            $gpart_wipe__cmd
            ;;
        *)
            return 1
            ;;
    esac
}

gpart_root()
{
    gpart_root__boot=

    OPTIND=1
    while getopts :b: opt; do
        gpart_root__boot="$OPTARG"
    done
    shift $(($OPTIND - 1))

    [ -z "$gpart_root__boot" ] && {
        echo >&2 "gpart_root: specify a boot mode, gpart_root -b <bootmode>"
        return 1
    }

    for gpart_root__d; do
        gpart_gpt "$gpart_root__d" || return 1

        case "$gpart_root__boot" in
            uefi|efi) gpart_root_add_efi "$gpart_root__d" || return 1 ;;
            bios) gpart_root_add_bios "$gpart_root__d" || return 1 ;;
        esac

        gpart_root_add_zfs "$gpart_root__d" root || return 1
    done
}

gpart_gpt()
{
    gpart create -s gpt "$1" || return 1
}

gpart_root_add_efi()
{
    gpart add -a 4k -s 200M -l "$1-efi" -t efi "$1"
}

gpart_root_add_bios()
{
    gpart add -a 4k -s 512k -l "$1-boot" -t freebsd-boot "$1"
}

gpart_root_add_zfs()
{
    gpart add -a 4k -l "$1-${2:-data}" -t freebsd-zfs "$1"
}


### ZFS

zfs_enable()
{
    grep 'zfs_enable="YES"' /etc/rc.conf ||
        echo 'zfs_enable="YES"' >> /etc/rc.conf
}

zfs_setup()
{
    kldstat -m zfs || kldload zfs || return 1
    sysctl vfs.zfs.min_auto_ashift=12 || return 1
}

zfs_root_mirror()
{
    zfs_root_mirror__mountpoint=/mnt
    zfs_root_mirror__pool=zroot

    OPTIND=1
    while getopts :m:p: opt; do
        case $opt in
            m) zfs_root_mirror__mountpoint="$OPTARG" ;;
            p) zfs_root_mirror__pool="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$zfs_root_mirror__mountpoint" ] && {
        echo >&2 "zfs_root_mirror: specify a mountpoint"
        return 1
    }
    [ -z "$zfs_root_mirror__pool" ] && {
        echo >&2 "zfs_root_mirror: specify a pool name"
        return 1
    }

    zfs_root_mirror__ds=
    for zfs_root_mirror__d; do
        zfs_root_mirror__ds="$zfs_root_mirror__ds /dev/gpt/${zfs_root_mirror__d}-root"
    done
    zpool create -f -o altroot=$zfs_root_mirror__mountpoint -O compress=lz4 -O atime=off -m none $zfs_root_mirror__pool mirror $zfs_root_mirror__ds
}

zfs_root()
{
    zfs_setup || return 1

    zfs_root__mountpoint=/mnt
    zfs_root__pool=zroot
    zfs_root__raid=

    OPTIND=1
    while getopts :m:p:r: opt; do
        case $opt in
            m) zfs_root__mountpoint="$OPTARG" ;;
            p) zfs_root__pool="$OPTARG" ;;
            r) zfs_root__raid="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$zfs_root__mountpoint" ] && {
        echo >&2 "zfs_root: specify a mountpoint"
        return 1
    }
    [ -z "$zfs_root__pool" ] && {
        echo >&2 "zfs_root: specify a pool name"
        return 1
    }
    [ -z "$zfs_root__raid" ] && {
        echo >&2 "zfs_root: specify a raid mode"
        return 1
    }

    case $zfs_root__raid in
        mirror) zfs_root_mirror -m "$zfs_root__mountpoint" -p "$zfs_root__pool" "$@" || return 1 ;;
        *) echo >&2 "not supported zfs_root mode: $zfs_root"
           return 1
           ;;
    esac

    zfs_root_datasets -p "$zfs_root__pool" -m "$zfs_root__mountpoint"
}

zfs_root_datasets()
{
    zfs_root_datasets__mountpoint=/mnt
    zfs_root_datasets__pool=zroot

    OPTIND=1
    while getopts :m:p: opt; do
        case $opt in
            m) zfs_root_datasets__mountpoint="$OPTARG" ;;
            p) zfs_root_datasets__pool="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$zfs_root_datasets__mountpoint" ] && {
        echo >&2 "zfs_root_datasets: need a mountpoint"
        return 1
    }
    [ -z "$zfs_root_datasets__pool" ] && {
        echo >&2 "zfs_root_datasets: need a pool name"
        return 1
    }

    zfs create -o mountpoint=none                      $zfs_root_datasets__pool/os
    zfs create -o mountpoint=/                         $zfs_root_datasets__pool/os/fbsd

    # zfs mount $zfs_root_datasets__pool/os/fbsd

    zfs create -o exec=on -o setuid=off -o quota=250m  $zfs_root_datasets__pool/os/fbsd/tmp

    zfs create                                         $zfs_root_datasets__pool/os/fbsd/usr
    zfs create                                         $zfs_root_datasets__pool/os/fbsd/usr/local
    zfs create -o setuid=off                           $zfs_root_datasets__pool/os/fbsd/usr/ports
    zfs create -o setuid=off                           $zfs_root_datasets__pool/os/fbsd/usr/ports/packages
    zfs create                                         $zfs_root_datasets__pool/os/fbsd/usr/src
    zfs create                                         $zfs_root_datasets__pool/os/fbsd/var
    zfs create -o exec=off -o setuid=off               $zfs_root_datasets__pool/os/fbsd/var/audit
    zfs create -o exec=off -o setuid=off               $zfs_root_datasets__pool/os/fbsd/var/crash
    zfs create                                         $zfs_root_datasets__pool/os/fbsd/var/db
    zfs create                                         $zfs_root_datasets__pool/os/fbsd/var/db/pkg
    zfs create                                         $zfs_root_datasets__pool/os/fbsd/var/empty
    zfs create -o exec=off -o setuid=off               $zfs_root_datasets__pool/os/fbsd/var/log
    zfs create                                         $zfs_root_datasets__pool/os/fbsd/var/mail
    zfs create                                         $zfs_root_datasets__pool/os/fbsd/var/run
    zfs create -o setuid=off                           $zfs_root_datasets__pool/os/fbsd/var/tmp
    zfs create -o mountpoint=/home                     $zfs_root_datasets__pool/home
    zfs create                                         $zfs_root_datasets__pool/home/root
    zfs create                                         $zfs_root_datasets__pool/home/admin
    zfs create -o mountpoint=/service                  $zfs_root_datasets__pool/service

    zfs set reservation=50G                            $zfs_root_datasets__pool/os
    zfs set reservation=25G                            $zfs_root_datasets__pool/os/fbsd
    zfs set quota=15G                                  $zfs_root_datasets__pool/os/fbsd/var/log

    (cd $zfs_root_datasets__mountpoint/usr && ln -sf /home)

    chmod 0700 $zfs_root_datasets__mountpoint/root
    chmod 0700 $zfs_root_datasets__mountpoint/home/admin
    chmod 1777 $zfs_root_datasets__mountpoint/tmp
    chmod 1777 $zfs_root_datasets__mountpoint/var/tmp

    zpool set bootfs=$zfs_root_datasets__pool/os/fbsd $zfs_root_datasets__pool
}


### pw

pw_user_a()
{
    pw_user_a__user=admin
    pw_user_a__uid=2001
    pw_user_a__shell=/bin/sh

    pw group show admin ||
        pw group add -n $pw_user_a__user -g $pw_user_a__uid ||
        return 1

    pw user show admin ||
        pw user add -n $pw_user_a__user -c $pw_user_a__user -u $pw_user_a__uid -g $pw_user_a__uid -s /bin/sh -w no ||
        return 1

    pw groupmod wheel -m admin || return 1

    mkdir -p /home/admin/.ssh

    chown -R admin:admin /home/admin || return 1
    chmod 0700 /home/admin || return 1
    chmod 0700 /home/admin/.ssh || return 1

    mkdir -p /usr/local/etc/sudoers.d
    echo '%admin ALL=(ALL) NOPASSWD: ALL' > /usr/local/etc/sudoers.d/admin
    chmod 640 /usr/local/etc/sudoers.d/admin
}


### pkg

pkg_install_minimum()
{
    pkg install -y python sudo
}


### FreeBSD

freebsd_root()
{
    freebsd_root__usage="-h -b <boottype> -m <mountpoint> -r <raidmode>"
    freebsd_root__mountpoint="/mnt"
    freebsd_root__type="zfs"
    freebsd_root__boot="uefi"
    freebsd_root__raid=
    freebsd_root__pool="zroot"

    OPTIND=1
    while getopts :hb:m:p:r: opt; do
        case $opt in
            b) freebsd_root__boot="$OPTARG" ;;
            m) freebsd_root__mountpoint="$OPTARG" ;;
            p) freebsd_root__pool="$OPTARG" ;;
            r) freebsd_root__raid="$OPTARG" ;;
            h) echo >&2 freebsd_root__usage ;;
        esac
    done
    shift $(($OPTIND - 1))

    if [ -z "$freebsd_root__raid" ]; then
        case $# in
            1) freebsd_root__raid=single ;;
            2) freebsd_root__raid=mirror ;;
            3) freebsd_root__raid=raid5 ;;
            *)
                echo >&2 "Please specify a raid mode"
                return 1
                ;;
        esac
    fi

    # 1. wipe
    gpart_wipes "$@" || return 1

    # 2. partition
    gpart_root -b "$freebsd_root__boot" "$@" || return 1

    # 3. fs
    zfs_root -m "$freebsd_root__mountpoint" -p "$freebsd_root__pool" -r "$freebsd_root__raid" "$@"
}

freebsd_bootcode()
{
    mount_devfs || return 1

    for freebsd_bootcode__d; do
        freebsd_bootcode__entry=$(gpart show | grep " efi ")
        if [ -n "$freebsd_bootcode__entry" ]; then
            freebsd_bootcode__i=${freebsd_bootcode__entry% * efi *}
            freebsd_bootcode__i=${freebsd_bootcode__i##* }

            freebsd_bootcode__cmd="gpart bootcode -p /boot/boot1.efifat -i $freebsd_bootcode__i $freebsd_bootcode__d"
            echo >&2 "execute: \"$freebsd_bootcode__cmd\" ? (y/N)"
            read answer
            case $answer in
                [Yy]|[Yy][Ee][Ss]) $freebsd_bootcode__cmd ;;
            esac
        fi

        freebsd_bootcode__entry=$(gpart show | grep " freebsd-boot ")
        if [ -n "$freebsd_bootcode__entry" ]; then
            freebsd_bootcode__i=${freebsd_bootcode__entry% * freebsd-boot *}
            freebsd_bootcode__i=${freebsd_bootcode__i##* }

            freebsd_bootcode__cmd="gpart bootcode -b /boot/pmbr -p /boot/gptzfsboot -i $freebsd_bootcode__i $freebsd_bootcode__d"
            echo >&2 "execute: \"$freebsd_bootcode__cmd\" ? (y/N)"
            read answer
            case $answer in
                [Yy]|[Yy][Ee][Ss]) $freebsd_bootcode__cmd ;;
            esac
        fi
    done
}

freebsd_bootstrap()
{
    zfs_enable &&
        pw_user_a "$@" &&
        pkg_install_minimum
}

freebsd()
{
    freebsd__action="$1"
    shift

    case "$freebsd__action" in
        root)      freebsd_root "$@" ;;
        bootcode)  freebsd_bootcode "$@" ;;
        bootstrap) freebsd_bootstrap "$@" ;;
        *)
            echo >&2 "unsupported freebsd action \"$freebsd_action\""
            return 1
            ;;
    esac
}


### main

case $SCRIPT_NAME in
    freebsd|freebsd.sh)
        freebsd "$@"
        ;;
esac


### ToDo
# - add tmpfs in /mnt before mounting FS
#   mount -t tmpfs tmpfs /mnt
