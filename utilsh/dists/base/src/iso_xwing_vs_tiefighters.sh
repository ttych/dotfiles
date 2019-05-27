#!/bin/sh

ISO_DIR="${ISO_DIR:-$HOME/save/iso}"

img_mount()
{
    cdemu load 0 "$1"
    sudo mount /dev/sr0 /mnt/cd
}

img_umount()
{
    sudo umount /mnt/cd
    sudo umount /dev/sr0
    cdemu unload 0
    sleep 2
}

img_umount 2>/de/vnull

cd "$ISO_DIR/star_wars"

# 1
echo 'X-Wing vs Tie Fighter'
img_mount x-wing_vs._tie-fighter__cd1.cue

# 2
echo 'Next ?'
read next
img_umount
echo 'Balance of Power'
img_mount x-wing_vs._tie-fighter__balance-of-power.cue

# 3
echo 'Quit ?'
read next
img_umount
