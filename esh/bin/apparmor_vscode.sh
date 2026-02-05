#!/bin/sh
# -*- mode: sh -*-


VSCODE_PATH="$1"

if [ -z "$VSCODE_PATH" ] || [ ! -d "$VSCODE_PATH" ]; then
    echo >&2 "invalid vscode path \"$VSCODE_PATH\""
    exit 1
fi

cat <<EOF
# content of /etc/apparmor.d/vscode_${USER}_1

abi <abi/4.0>,
include <tunables/global>

profile vscode_${USER}_1 ${VSCODE_PATH%/}{/bin,}/{code,codium} flags=(unconfined) {
  userns,

  include if exists <local/vscode_${USER}_1>
}

# reload with :
#   apparmor_parser -r /etc/apparmor.d/vscode_${USER}_1
#   systemctl reload apparmor
EOF
