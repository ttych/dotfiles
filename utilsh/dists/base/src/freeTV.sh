#!/bin/sh

device=`ip link | grep 'state UP' | awk '{print $2}'`
device="${device%:*}"
[ -z "$device" ] && exit 1

ip=`ip addr show ${device} |grep 'inet ' | awk '{print $2}'`
ip="${ip%/*}"

port_num=$(echo $ip | cut -d. -f4)
port_num=$(($port_num * 10 + 60000 + 6))

vlc --rtp-client-port $port_num http://mafreebox.freebox.fr/freeboxtv/playlist.m3u
