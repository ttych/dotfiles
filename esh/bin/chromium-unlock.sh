#!/bin/sh

for chromium_conf_dir in \
    "$HOME/.config/google-chrome/" \
        "$HOME/snap/chromium/common/chromium/ " ;
do
    [ -d "$chromium_conf_dir" ] || continue
    rm -Rf "$chromium_conf_dir"/Singleton*
done
