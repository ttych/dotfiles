#!/bin/sh

# set -x

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


########## options

FBSD_SYS_POOL=${FBSD_SYS_POOL:-zsys}

FBSD_BIOS_SIZE=${FBSD_BIOS_SIZE:-512k}
FBSD_EFI_SIZE=${FBSD_EFI_SIZE:-128M}

FBSD_EXEC_CHECK=${FBSD_EXEC_CHECK:-yes}
FBSD_VERBOSE=${FBSD_VERBOSE:-yes}

fbsd_verbose()
{
    [ -n "$FBSD_VERBOSE" ]
}

fbsd_verbose_command()
{
    fbsd_verbose || return 0

    fbsd_verbose_command="$1"; shift
    cat2 <<EOF
$fbsd_verbose_command> $fbsd_verbose_command $@
EOF
}

fbsd_check()
{
    [ -n "$FBSD_EXEC_CHECK" ]
}

echo2()
{
    echo >&2 "$@"
}

cmd_err_msg()
{
    cmd_err__title="$1"; shift

    echo2 "$cmd_err__title>" "$@"
}

cat2()
{
    cat >&2 "$@"
}

########## exec
fbsd_exec()
{
    fbsd_verbose && echo2 "exec> $@"

    if fbsd_check; then
        echo -n "exec> Yes / No / Skip  ? "
        read answer
        case $answer in
            [Yy]|[Yy][Ee][Ss]|"")
            ;;
            [Ss]|[Ss][Kk][Ii][Pp])
                echo2 "exec> SKIPPED"
                return 0
                ;;
            *)
                echo2 "exec> ABORTED"
                return 1
                ;;
        esac
    fi

    "$@"
}


########## dev
fbsd_dev_char_spec()
{
    [ -c "/dev/$1" ] || {
        fbsd_verbose && echo2 "$1 is not a character special"
        return 1
    }
}


########## disks
fbsd_disks()
{
    fbsd_disks="$(sysctl -n kern.disks | tr ' ' '\n')"
}

fbsd_disks_list()
{
    fbsd_disks &&
        echo "$fbsd_disks"
}

fbsd_disks_has()
{
    [ $# -eq 0 ] && return 1

    for fbsd_disks_has__d; do
        fbsd_disks_list | grep "$fbsd_disks_has__d" >/dev/null || {
            fbsd_verbose && echo2 "$fbsd_disks_has__d is not a valid disk"
            return  1
        }

        fbsd_dev_char_spec "$fbsd_disks_has__d" || return 1
    done
}


########## geom

fbsd_geom_list()
{
    geom -t
}

# fbsd_has_disk()
# {
#     [ $# -eq 0 ] && return 1

#     for fbsd_has_disk__disk; do
#         geom -t | grep " DISK " | grep "${fbsd_has_disk__disk}" >/dev/null || {
#             echo "$fbsd_has_disk__disk is not a valid disk"
#             return 1
#         }

#         [ -c "/dev/${fbsd_has_disk__disk}" ] || {
#             echo "$fbsd_has_disk__disk is not a character special disk"
#             return 1
#         }
#     done
# }

fbsd_geom_disk_list()
{
    geom disk list
}


########## bootmethod

fbsd_bootmethod()
{
    fbsd_bootmethod="$(fbsd_sysctl_value machdep.bootmethod)"
    echo $fbsd_bootmethod
}


########## sysctl

fbsd_sysctl()
{
    sysctl "$@"
}

fbsd_sysctl_value()
{
    fbsd_sysctl -n "$@"
}

fbsd_sysctl_description()
{
    fbsd_sysctl -d "$@"
}

fbsd_sysctl_zfs()
{
    fbsd_sysctl vfs.zfs.min_auto_ashift=12 || return 1
}


########## kernel loader

fbsd_kld_load()
{
    kldstat -m "$1" 2>/dev/null >/dev/null || kldload "$1" || return 1
}

fbsd_kld_zfs()
{
    fbsd_kld_load zfs
}


########## gpart

fbsd_gpart_wipe()
{
    for fbsd_gpart_wipe__disk; do
        fbsd_disks_has "$fbsd_gpart_wipe__disk" || {
            echo2 "$fbsd_gpart_wipe__disk cannot be wiped"
            return 1
        }

        gpart show "$fbsd_gpart_wipe__disk" 2>/dev/null >/dev/null || return 0

        fbsd_exec gpart destroy -F "$fbsd_gpart_wipe__disk"
    done
}

fbsd_gpart_gpt()
{
    fbsd_exec gpart create -s gpt "$1" || return 1
}

fbsd_gpart_add_efi()
{
    fbsd_exec gpart add -a 4k -s ${FBSD_EFI_SIZE} -l "efi" -t efi "$1"
}

fbsd_gpart_add_bios()
{
    fbsd_exec gpart add -a 4k -s ${FBSD_BIOS_SIZE} -l "boot" -t freebsd-boot "$1"
}

fbsd_gpart_add_zfs()
{
    fbsd_exec gpart add -a 4k -l "${2:-zfs}" -t freebsd-zfs "$1"
}

fbsd_gpart_format_sys()
{
    fbsd_gpart_format_sys__cmd="gpart_format_sys"
    fbsd_gpart_format_sys__usage="-h -b bootmethod disk1 .. diskn"
    fbsd_gpart_format_sys__boot=

    OPTIND=1
    while getopts :hb: opt; do
        case $opt in
            h) fbsd_usage_command "$fbsd_gpart_format_sys__cmd" "$fbsd_gpart_format_sys__usage"
               return 0 ;;
            b) fbsd_gpart_format_sys__boot="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$fbsd_gpart_format_sys__boot" ] && {
        echo2 "bootmethod is missing in $fbsd_gpart_format_sys__cmd"
        return 1
    }

    [ $# -eq 0 ] && {
        echo2 "missing disk for $fbsd_gpart_format_sys__cmd"
        return 1
    }

    fbsd_gpart_format_sys__i=0
    for fbsd_gpart_format_sys__d; do
        fbsd_disks_has "$fbsd_gpart_format_sys__d" || return 1
        fbsd_gpart_wipe "$fbsd_gpart_format_sys__d" || return 1
        fbsd_gpart_gpt "$fbsd_gpart_format_sys__d" || return 1
        case "$fbsd_gpart_format_sys__boot" in
            uefi|efi) fbsd_gpart_add_efi "$fbsd_gpart_format_sys__d" "$fbsd_gpart_format_sys__i" || return 1 ;;
            bios|mbr) fbsd_gpart_add_bios "$fbsd_gpart_format_sys__d" "$fbsd_gpart_format_sys__i" || return 1 ;;
        esac
        fbsd_gpart_add_zfs "$fbsd_gpart_format_sys__d" sys "$fbsd_gpart_format_sys__i" || return 1

        fbsd_gpart_format_sys__i=$(($fbsd_gpart_format_sys__i + 1))
    done
}


########## mount

fbsd_mount_devfs()
{
    if df /dev | tail -1 | grep -q '^devfs'; then
        :
    else
        mount -t devfs devfs /dev || return 1
    fi
}


########## pkg

pkg_install()
{
    pkg install -y "$@"
}


########## rc

fbsd_rc_enable()
{
   grep "${1}_enable=\"YES\"" /etc/rc.conf ||
        echo "${1}_enable=\"YES\"" >> /etc/rc.conf
}


########## zfs

fbsd_zfs_context()
{
    fbsd_kld_zfs &&
        fbsd_sysctl_zfs
}

fbsd_zfs_zpool_create()
{
    fbsd_exec zpool create -f -O compress=lz4 -O atime=off -m none "$@"
}

fbsd_zfs_pool_mirror()
{
    fbsd_zfs_pool_mirror__mountpoint=
    fbsd_zfs_pool_mirror__pool=

    OPTIND=1
    while getopts :m:p: opt; do
        case $opt in
            m) fbsd_zfs_pool_mirror__mountpoint="$OPTARG" ;;
            p) fbsd_zfs_pool_mirror__pool="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$fbsd_zfs_pool_mirror__mountpoint" ] && fbsd_zfs_pool_mirror__mountpoint="/mnt"
    [ -z "$fbsd_zfs_pool_mirror__pool" ] && fbsd_zfs_pool_mirror__pool="zpool"

    fbsd_zfs_zpool_create -o altroot="$fbsd_zfs_pool_mirror__mountpoint" "$fbsd_zfs_pool_mirror__pool" mirror "$@"
}

fbsd_zfs_pool_sys()
{
    fbsd_zfs_pool_sys__cmd="zfs_pool_sys"
    fbsd_zfs_pool_sys__usage="-h -m <mountpoint> -r <raidmode> -p <poolname>"
    fbsd_zfs_pool_sys__mountpoint=
    fbsd_zfs_pool_sys__raid=
    fbsd_zfs_pool_sys__pool=

    OPTIND=1
    while getopts :hm:r:p: opt; do
        case $opt in
            h) fbsd_usage_command "$fbsd_zfs_pool_sys__cmd" "$fbsd_zfs_pool_sys__usage" ;;
            m) fbsd_zfs_pool_sys__mountpoint="$OPTARG" ;;
            r) fbsd_zfs_pool_sys__raid="$OPTARG" ;;
            p) fbsd_zfs_pool_sys__pool="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$fbsd_zfs_pool_sys__mountpoint" ] && fbsd_zfs_pool_sys__mountpoint="/mnt"
    [ -z "$fbsd_zfs_pool_sys__raid" ] && {
        cmd_err_msg "$fbsd_zfs_pool_sys__cmd" "raid can not be empty"
        return 1
    }
    [ -z "$fbsd_zfs_pool_sys__pool" ] && {
        cmd_err_msg "$fbsd_zfs_pool_sys__cmd" "pool name can not be empty"
        return 1
    }

    if fbsd_verbose; then
        fbsd_verbose_command \
            "$fbsd_zfs_pool_sys__cmd" \
            -m "$fbsd_zfs_pool_sys__mountpoint" \
            -r "$fbsd_zfs_pool_sys__raid" \
            -p "$fbsd_zfs_pool_sys__pool" \
            "$@"
    fi

    fbsd_zfs_context || return 1

    case $fbsd_zfs_pool_sys__raid in
        standalone|single|mirror|raid5)
            fbsd_zfs_pool_"$fbsd_zfs_pool_sys__raid" \
                          -m "$fbsd_zfs_pool_sys__mountpoint" \
                          -p "$fbsd_zfs_pool_sys__pool" \
                          "$@"
            ;;
        *) cmd_err_msg "$fbsd_zfs_pool_sys__cmd" "$fbsd_zfs_pool_sys__raid is not a valid raid mode" ;;
    esac

    fbsd_zfs_datasets_sys -p "$fbsd_zfs_pool_sys__pool" -m "$fbsd_zfs_pool_sys__mountpoint" || return 1
}

fbsd_zfs_datasets_sys()
{
    OPTIND=1
    while getopts :m:p: opt; do
        case $opt in
            m) fbsd_zfs_datasets_sys__mountpoint="$OPTARG" ;;
            p) fbsd_zfs_datasets_sys__pool="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$fbsd_zfs_datasets_sys__mountpoint" ] && fbsd_zfs_datasets_sys__mountpoint="/mnt"
    [ -z "$fbsd_zfs_datasets_sys__pool" ] && fbsd_zfs_datasets_sys__pool="$FBSD_SYS_POOL"

    fbsd_exec zfs create -o mountpoint=none                      $fbsd_zfs_datasets_sys__pool/sys
    fbsd_exec zfs create -o mountpoint=/                         $fbsd_zfs_datasets_sys__pool/sys/fbsd

    # zfs mount $fbsd_zfs_datasets_sys__pool/sys/fbsd

    fbsd_exec zfs create -o exec=on -o setuid=off -o quota=500m  $fbsd_zfs_datasets_sys__pool/sys/fbsd/tmp

    fbsd_exec zfs create -o canmount=off                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/usr
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/usr/local
    fbsd_exec zfs create -o setuid=off                           $fbsd_zfs_datasets_sys__pool/sys/fbsd/usr/ports
    fbsd_exec zfs create -o setuid=off                           $fbsd_zfs_datasets_sys__pool/sys/fbsd/usr/ports/packages
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/usr/src
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/var
    fbsd_exec zfs create -o exec=off -o setuid=off               $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/audit
    fbsd_exec zfs create -o exec=off -o setuid=off               $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/crash
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/db
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/db/pkg
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/empty
    fbsd_exec zfs create -o exec=off -o setuid=off               $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/log
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/mail
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/run
    fbsd_exec zfs create -o setuid=off                           $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/tmp
    fbsd_exec zfs create -o mountpoint=/home                     $fbsd_zfs_datasets_sys__pool/home
    fbsd_exec zfs create -o mountpoint=/root                     $fbsd_zfs_datasets_sys__pool/home/root
    fbsd_exec zfs create                                         $fbsd_zfs_datasets_sys__pool/home/admin
    fbsd_exec zfs create -o mountpoint=/service                  $fbsd_zfs_datasets_sys__pool/service

    fbsd_exec zfs set reservation=50G                            $fbsd_zfs_datasets_sys__pool/sys
    fbsd_exec zfs set reservation=25G                            $fbsd_zfs_datasets_sys__pool/sys/fbsd
    fbsd_exec zfs set quota=15G                                  $fbsd_zfs_datasets_sys__pool/sys/fbsd/var/log

    (cd $fbsd_zfs_datasets_sys__mountpoint/usr && ln -sf /home)

    fbsd_exec chmod 0700 $fbsd_zfs_datasets_sys__mountpoint/root
    fbsd_exec chmod 0700 $fbsd_zfs_datasets_sys__mountpoint/home/admin
    fbsd_exec chmod 1777 $fbsd_zfs_datasets_sys__mountpoint/tmp
    fbsd_exec chmod 1777 $fbsd_zfs_datasets_sys__mountpoint/var/tmp

    fbsd_exec zpool set bootfs=$fbsd_zfs_datasets_sys__pool/sys/fbsd $fbsd_zfs_datasets_sys__pool
}

fbsd_zfs_enable()
{
    fbsd_rc_enable zfs
}


########## command: sys_disk
fbsd_sys_disk()
{
    fbsd_sys_disk__cmd="sys-disk"
    fbsd_sys_disk__usage="-h -b bootmethod -m <mountpoint> -r <raidmode> disk1 .. diskn"
    fbsd_sys_disk__type=
    fbsd_sys_disk__boot="uefi"
    fbsd_sys_disk__mountpoint=
    fbsd_sys_disk__raid=
    fbsd_sys_disk__zfs_pool=

    OPTIND=1
    while getopts :ht:b:m:r:p: opt; do
        case $opt in
            h) fbsd_usage_command "$fbsd_sys_disk__cmd" "$fbsd_sys_disk__usage"
               return 0 ;;
            t) fbsd_sys_disk__type="$OPTARG" ;;
            b) fbsd_sys_disk__boot="$OPTARG" ;;
            m) fbsd_sys_disk__mountpoint="$OPTARG" ;;
            r) fbsd_sys_disk__raid="$OPTARG" ;;
            p) fbsd_sys_disk__zfs_pool="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    if [ $# -eq 0 ]; then
        echo2 "please specify disk from: "
        fbsd_disks_list
        return 1
    fi

    [ -z "$fbsd_sys_disk__type" ] && fbsd_sys_disk__type="zfs"
    [ -z "$fbsd_sys_disk__boot" ] && fbsd_sys_disk__boot="$(fbsd_bootmethod | tr 'A-Z' 'a-z')"
    [ -z "$fbsd_sys_disk__mountpoint" ] && fbsd_sys_disk__mountpoint="/mnt"
    if [ -z "$fbsd_sys_disk__raid" ]; then
        case $# in
            1) fbsd_sys_disk__raid=single ;;
            2) fbsd_sys_disk__raid=mirror ;;
            3) fbsd_sys_disk__raid=raid5 ;;
            *) echo2 "please choose a raid mode"
               return 1 ;;
        esac
    fi
    [ -z "$fbsd_sys_disk__zfs_pool" ] && fbsd_sys_disk__zfs_pool="$FBSD_SYS_POOL"

    if fbsd_verbose; then
        fbsd_verbose_command \
            "$fbsd_sys_disk__cmd" \
            -t "$fbsd_sys_disk__type" \
            -b "$fbsd_sys_disk__boot" \
            -m "$fbsd_sys_disk__mountpoint" \
            -r "$fbsd_sys_disk__raid" \
            -p "$fbsd_sys_disk__zfs_pool" \
            "$@"
    fi

    fbsd_gpart_format_sys -b "$fbsd_sys_disk__boot" "$@" || return 1

    fbsd_sys_disk__pool_disks=
    for fbsd_sys_disk__d in "$@"; do
        fbsd_sys_disk__pool_disks="$fbsd_sys_disk__pool_disks ${fbsd_sys_disk__d}p2"
    done
    fbsd_zfs_pool_sys -m "$fbsd_sys_disk__mountpoint" -p "$fbsd_sys_disk__zfs_pool" -r "$fbsd_sys_disk__raid" $fbsd_sys_disk__pool_disks || return 1
}


########## command: bootcode
fbsd_bootcode()
{
    fbsd_mount_devfs || return 1

    if [ $# -eq 0 ]; then
        echo2 "please specify disk from: "
        fbsd_disks_list
        return 1
    fi

    for fbsd_bootcode__d; do
        fbsd_bootcode__entry=$(gpart show "$fbsd_bootcode__d" | grep " efi ")
        if [ -n "$fbsd_bootcode__entry" ]; then
            fbsd_bootcode__i=${fbsd_bootcode__entry% * efi *}
            fbsd_bootcode__i=${fbsd_bootcode__i##* }

            fbsd_exec newfs_msdos -F 32 -c 1 "/dev/${fbsd_bootcode__d}p${fbsd_bootcode__i}" &&
                mkdir -p "/boot/boot__${fbsd_bootcode__d}p${fbsd_bootcode__i}" &&
                mount -t msdosfs -o longnames "/dev/${fbsd_bootcode__d}p${fbsd_bootcode__i}" "/boot/boot__${fbsd_bootcode__d}p${fbsd_bootcode__i}" &&
                mkdir -p "/boot/boot__${fbsd_bootcode__d}p${fbsd_bootcode__i}/EFI/BOOT" &&
                cp /boot/loader.efi "/boot/boot__${fbsd_bootcode__d}p${fbsd_bootcode__i}/EFI/BOOT/BOOTX64.efi" &&
                umount "/boot/boot__${fbsd_bootcode__d}p${fbsd_bootcode__i}" &&
                rmdir "/boot/boot__${fbsd_bootcode__d}p${fbsd_bootcode__i}"
        fi

        fbsd_bootcode__entry=$(gpart show "$fbsd_bootcode__d" | grep " freebsd-boot ")
        if [ -n "$fbsd_bootcode__entry" ]; then
            fbsd_bootcode__i=${fbsd_bootcode__entry% * freebsd-boot *}
            fbsd_bootcode__i=${fbsd_bootcode__i##* }

            fbsd_exec gpart bootcode -b /boot/pmbr -p /boot/gptzfsboot -i $fbsd_bootcode__i $fbsd_bootcode__d
        fi
    done
}


########## command: bootstrap
fbsd_bootstrap()
{
    pkg_install python sudo
}


########## main

fbsd_usage()
{
    cat >&2 <<EOF
usage is:
    $0 <g-options> <command> <c-options> <c-args>

with g-options in:
    -h    : help
    -v/-V : verbose / non-verbose
    -x/-X : debug / non-debug
    -c/-C : check before exec / non-check

with command in:
    help       : display help
    sys-disk   : prepare system disk to receive freebsd
    bootcode   : install bootcode on selected disk
    bootstrap  : install minimum packages
EOF
}

fbsd_usage_command()
{
    fbsd_usage

    cat >&2 <<EOF

command $1:
    $2
EOF
}

fbsd_usage_exit()
{
    fbsd_usage
    exit ${1:-1}
}

fbsd()
{
    OPTIND=1
    while getopts :vVxXcCh opt; do
        case $opt in
            v) FBSD_VERBOSE=yes ;;
            V) FBSD_VERBOSE= ;;
            x) set -x ;;
            X) set +x ;;
            c) FBSD_EXEC_CHECK=yes ;;
            C) FBSD_EXEC_CHECK= ;;
            h) fbsd_usage_exit 0
        esac
    done
    shift $(($OPTIND - 1))

    [ $# -eq 0 ] && fbsd_usage_exit
    fbsd__action="$(echo $1 | tr '-' '_')" ; shift
    case "$fbsd__action" in
        help) fbsd_usage_exit 0 ;;
        sys_disk|bootcode|bootstrap)
            "fbsd_${fbsd__action}" "$@" ;;
        *) fbsd_usage_exit ;;
    esac
}


case $SCRIPT_NAME in
    fbsd|fbsd.sh)
        fbsd "$@"
        ;;
esac




########## references
##### install
# base commands list to install
# - https://wiki.freebsd.org/RootOnZFS/GPTZFSBoot
# commands list to install
# - https://forums.freebsd.org/threads/uefi-gpt-dual-boot-how-to-install-freebsd-with-zfs-alongside-another-os-sharing-the-same-disk.75734/
# Converting FreeBSD from bios / mbr to UEFI / GPT
# - https://just.graphica.com.au/tips/converting-freebsd-bios/
#####
