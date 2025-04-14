#!/bin/sh

# From:
# https://docs.zfsbootmenu.org/en/latest/guides/ubuntu/uefi.html

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


######################################### env

PKG_REQ="debootstrap gdisk zfsutils-linux mdadm"
BOOT_PART=1
POOL_PART=2
POOL_NAME="${POOL_NAME:-zroot}"


######################################### utils

TRUE()
{
    return 0
}

FALSE()
{
    return 1
}

echo2()
{
    echo >&2 "$@"
}

log()
{
    log__context="[ ] "
    if [ $# -gt 1 ]; then
        log__context="[$1] "
        shift
    fi

    echo2 "$log__context""$@"
}

check_efi()
{
    [ -d /sys/firmware/efi ]
}


######################################### apt

apt_update()
{
    apt -q -y update
}

apt_install()
{
    apt_update && \
        apt -q -y install "$@"
}


######################################### zfs

zfs_destroy()
{
    if zfs list "$@" >/dev/null; then
        zfs destroy "$@"
    fi
}

zfs_have_dataset()
{
    zfs_dataset="$1" ; shift

    if ! zfs list "$zfs_dataset" >/dev/null; then
        zfs create "$@" "$zfs_dataset"
    fi
}


######################################### steps
step_prereq()
{
    if ! check_efi; then
        log prereq "not an EFI boot"
        return 1
    fi

    apt_install $PKG_REQ
}

step_env()
{
    . /etc/os-release
    export ID
}

step_wipe_disk()
{
    zpool labelclear -f "$1"
    wipefs -a "$1"
    sgdisk --zap-all "$1"
}

step_part_disk()
{
    sgdisk -n "${BOOT_PART}:1m:+512m" -t "${BOOT_PART}:ef00" "$1"
    sgdisk -n "${POOL_PART}:0:-10m" -t "${POOL_PART}:bf00" "$1"
}

step_zpool()
{
    step_zpool__type=
    step_zpool__disk1=
    step_zpool__disk2=
    if [ -n "$1" ] && [ -n "$2" ]; then
        step_zpool__type=mirror
    fi
    if [ -n "$1" ]; then
        step_zpool__disk1=${1}?p???${POOL_PART}
    fi
    if [ -n "$2" ]; then
        step_zpool__disk2=${2}?p???${POOL_PART}
    fi

    zpool create -f \
          -o ashift=12 \
          -o autotrim=on \
          -O compression=lz4 \
          -O acltype=posixacl \
          -O xattr=sa \
          -O dnodesize=auto \
          -O normalization=formD \
          -O atime=off \
          -O relatime=off \
          -m none \
          "$POOL_NAME" $step_zpool__type $step_zpool__disk1 $step_zpool__disk2
}

step_zpool_datasets()
{
    zfs_destroy "$POOL_NAME/ROOT/${ID}"
    zfs_have_dataset "$POOL_NAME/ROOT" -o mountpoint=none &&
        zfs_have_dataset "$POOL_NAME/ROOT/${ID}" -o compression=lz4 -o mountpoint=/ -o canmount=noauto &&
        zfs_have_dataset "$POOL_NAME/DATA" -o compression=lz4 -o mountpoint=none -o atime=off -o relatime=off &&
        zfs_have_dataset "$POOL_NAME/DATA/home" -o compression=lz4 -o mountpoint=/home &&
        zfs_have_dataset "$POOL_NAME/DATA/home/root" &&
        zfs_have_dataset "$POOL_NAME/DATA/home/admin" &&
        zfs_have_dataset "$POOL_NAME/APP" -o compression=lz4 -o mountpoint=none -o atime=off -o relatime=off &&
        zfs_have_dataset "$POOL_NAME/APP/app" -o compression=lz4 -o mountpoint=/app &&
        zfs_have_dataset "$POOL_NAME/APP/service" -o compression=lz4 -o mountpoint=/service &&
        zpool set bootfs="$POOL_NAME/ROOT/${ID}" "$POOL_NAME"

        # zfs_have_dataset "$POOL_NAME/DATA/data" -o compression=lz4 -o mountpoint=/data &&
        # zfs_have_dataset "$POOL_NAME/DATA/share" -o compression=lz4 -o mountpoint=/share &&
}

step_zpool_import()
{
    zpool import -f -N -R /mnt "$POOL_NAME"
}

step_zpool_reimport()
{
    zpool export "$POOL_NAME" &&
        zpool import -N -R /mnt "$POOL_NAME" &&
        zfs mount "$POOL_NAME/ROOT/${ID}" &&
        zfs mount "$POOL_NAME/DATA/home"
}

step_update_symlinks()
{
    udevadm trigger
}

step_install_ubuntu()
{
    debootstrap "$UBUNTU_CODENAME" /mnt
}

step_prepare_postinstall()
{
    mount -t proc proc /mnt/proc
    mount -t sysfs sys /mnt/sys
    mount -B /dev /mnt/dev
    mount -t devpts pts /mnt/dev/pts
}

step_run_postinstall()
{
    cp "$0" "/mnt/var/tmp/$SCRIPT_NAME"

    chroot /mnt /bin/bash "/var/tmp/$SCRIPT_NAME" postinstall "$@"
}

step_post_host()
{
    echo "$HOSTNAME" > /etc/hostname
    echo -e "127.0.1.1\t$HOSTNAME" >> /etc/hosts
}

step_post_password()
{
    echo ROOT PASSWORD
    passwd
}

step_post_apt_source()
{
    . /etc/os-release

    cat <<EOF > /etc/apt/sources.list
# Uncomment the deb-src entries if you need source packages

deb http://archive.ubuntu.com/ubuntu/ ${UBUNTU_CODENAME} main restricted universe multiverse
# deb-src http://archive.ubuntu.com/ubuntu/ ${UBUNTU_CODENAME} main restricted universe multiverse

deb http://archive.ubuntu.com/ubuntu/ ${UBUNTU_CODENAME}-updates main restricted universe multiverse
# deb-src http://archive.ubuntu.com/ubuntu/ ${UBUNTU_CODENAME}-updates main restricted universe multiverse

deb http://archive.ubuntu.com/ubuntu/ ${UBUNTU_CODENAME}-security main restricted universe multiverse
# deb-src http://archive.ubuntu.com/ubuntu/ ${UBUNTU_CODENAME}-security main restricted universe multiverse

deb http://archive.ubuntu.com/ubuntu/ ${UBUNTU_CODENAME}-backports main restricted universe multiverse
# deb-src http://archive.ubuntu.com/ubuntu/ ${UBUNTU_CODENAME}-backports main restricted universe multiverse

#deb http://archive.canonical.com/ubuntu/ ${UBUNTU_CODENAME} partner
# deb-src http://archive.canonical.com/ubuntu/ ${UBUNTU_CODENAME} partner
EOF
}

step_post_apt_install()
{
    apt_install --no-install-recommends linux-generic locales keyboard-configuration console-setup || return 1
    dpkg-reconfigure locales tzdata keyboard-configuration console-setup || return 1
}

step_post_zfs_setup()
{
    apt_install dosfstools zfs-initramfs zfsutils-linux || return 1

    systemctl enable zfs.target
    systemctl enable zfs-import-cache
    systemctl enable zfs-mount
    systemctl enable zfs-import.target
}

step_post_initramfs()
{
    update-initramfs -c -k all
}

step_post_netplan()
{
    cat > /etc/netplan/01-netcfg.yaml <<EOF
---
network:
  version: 2
  renderer: networkd
  ethernets:
    $(ip -4 addr show up | awk '/inet/ {print $NF}' | grep -v lo | head -n 1):
      dhcp4: true
EOF
    netplan apply
}

step_post_zfs_boot_menu()
{
    apt_install curl || return 1

    zfs set org.zfsbootmenu:commandline="verbose" "$POOL_NAME/ROOT" || return 1

    if [ -n "$DISK1" ]; then
        BOOT_DISK1=$(realpath "$DISK1")
        if [ -r "${BOOT_DISK1}p${BOOT_PART}" ]; then
            BOOT_DEVICE1="${BOOT_DISK1}p${BOOT_PART}"
        elif [ -r "${BOOT_DISK1}${BOOT_PART}" ]; then
            BOOT_DEVICE1="${BOOT_DISK1}${BOOT_PART}"
        else
            log zfs_boot_menu "no BOOT_DEVICE1 found"
            exit 1
        fi

        mkfs.vfat -F32 "$BOOT_DEVICE1" || return 1
        cat << EOF >> /etc/fstab
$( blkid | grep "$BOOT_DEVICE1" | cut -d ' ' -f 2 ) /boot/efi vfat defaults 0 0
EOF
        mkdir -p /boot/efi
        mount /boot/efi
        mkdir -p /boot/efi/EFI/ZBM

        curl -o /boot/efi/EFI/ZBM/VMLINUZ.EFI -L https://get.zfsbootmenu.org/efi
        cp /boot/efi/EFI/ZBM/VMLINUZ.EFI /boot/efi/EFI/ZBM/VMLINUZ-BACKUP.EFI
    fi

    if [ -n "$DISK2" ]; then
        BOOT_DISK2=$(realpath "$DISK2")
        if [ -r "${BOOT_DISK2}p${BOOT_PART}" ]; then
            BOOT_DEVICE2="${BOOT_DISK2}p${BOOT_PART}"
        elif [ -r "${BOOT_DISK2}${BOOT_PART}" ]; then
            BOOT_DEVICE2="${BOOT_DISK2}${BOOT_PART}"
        else
            log zfs_boot_menu "no BOOT_DEVICE2 found"
            exit 1
        fi

        mkfs.vfat -F32 "$BOOT_DEVICE2" || return 1
        cat << EOF >> /etc/fstab
$( blkid | grep "$BOOT_DEVICE2" | cut -d ' ' -f 2 ) /boot/efi_alt vfat defaults 0 0
EOF
        mkdir -p /boot/efi_alt
        mount /boot/efi_alt
        mkdir -p /boot/efi_alt/EFI/ZBM

        cp /boot/efi/EFI/ZBM/VMLINUZ.EFI /boot/efi_alt/EFI/ZBM/VMLINUZ.EFI
        cp /boot/efi_alt/EFI/ZBM/VMLINUZ.EFI /boot/efi_alt/EFI/ZBM/VMLINUZ-BACKUP.EFI
    fi

    mount -t efivarfs efivarfs /sys/firmware/efi/efivars

    apt_install efibootmgr || return 1

    if [ -n "$DISK1" ]; then
        efibootmgr -c -d "$BOOT_DISK1" -p "$BOOT_PART" \
                   -L "ZFSBootMenu (Backup)" \
                   -l '\EFI\ZBM\VMLINUZ-BACKUP.EFI'

        efibootmgr -c -d "$BOOT_DISK1" -p "$BOOT_PART" \
                   -L "ZFSBootMenu" \
                   -l '\EFI\ZBM\VMLINUZ.EFI'
    fi

    if [ -n "$DISK2" ]; then
        efibootmgr -c -d "$BOOT_DISK2" -p "$BOOT_PART" \
                   -L "ZFSBootMenu alt (Backup)" \
                   -l '\EFI\ZBM\VMLINUZ-BACKUP.EFI'

        efibootmgr -c -d "$BOOT_DISK2" -p "$BOOT_PART" \
                   -L "ZFSBootMenu alt" \
                   -l '\EFI\ZBM\VMLINUZ.EFI'
    fi
}

step_post_base_packages()
{
    apt_install openssh-server ubuntu-minimal  # ubuntu-server-minimal
}

step_post_users()
{
    groupadd -g 2001 admin
    useradd -u 2001 -g 2001 admin

    cat > /etc/sudoers.d/admin <<EOF
%admin ALL=(ALL) NOPASSWD: ALL"
EOF
    chmod 640 /etc/sudoers.d/admin

    chown -R admin:admin /home/admin
    chmod 0700 /home/admin
}


######################################### install

zinstall()
{
    if [ $# -lt 2 ] || [ $# -gt 3 ]; then
        log zinstall "expecting 3 arguments: host disk1 disk2"
        return 1
    fi

    HOSTNAME="$1"
    DISK1="$2"
    DISK2="$3"

    step_prereq || return 1
    step_env || return 1

    if [ -z "$NOFORMAT" ] && [ -z "$NO_FORMAT" ]; then

        step_wipe_disk "$DISK1" || return 1
        if [ -n "$DISK2" ]; then
            step_wipe_disk "$DISK2" || return 1
        fi

        step_part_disk "$DISK1" || return 1
        if [ -n "$DISK2" ]; then
            step_part_disk "$DISK2" || return 1
        fi

        sleep 5

        step_zpool "$DISK1" "$DISK2" || return 1

    else

        step_zpool_import || return 1

    fi

    step_zpool_datasets || return 1

    step_zpool_reimport || return 1

    step_update_symlinks || return 1

    step_install_ubuntu || return 1

    step_prepare_postinstall || return 1

    step_run_postinstall "$HOSTNAME" "$DISK1" "$DISK2" || return 1
}

zpostinstall()
{
    if [ $# -lt 2 ] || [ $# -gt 3 ]; then
        log zpostinstall "expecting 3 arguments: host disk1 disk2"
        return 1
    fi

    HOSTNAME="$1"
    DISK1="$2"
    DISK2="$3"

    step_post_host || return 1

    step_post_password || return 1

    step_post_apt_source || return 1

    step_post_apt_install || return 1

    step_post_zfs_setup || return 1

    step_post_initramfs || return 1

    step_post_zfs_boot_menu || return 1

    step_post_netplan || return 1

    step_post_base_packages || return 1

    step_post_users || return 1
}


######################################### main

usage()
{
    cat <<EOF
Usage is :
    $SCRIPT_NAME <action> <action_args>

With action in :
    h  | help
    i  | install      <host> <disk1> <disk2>
    p  | postinstall  <host> <disk1> <disk2>
EOF
}

action="$1"
[ $# -gt 0 ] && shift
case "$action" in
    h|help|"")
        usage
        exit 0
        ;;
    i|install)
        zinstall "$@"
        exit $?
        ;;
    p|postinstall)
        zpostinstall "$@"
        exit $?
        ;;
     *)
        usage
        exit 1
        ;;
esac
