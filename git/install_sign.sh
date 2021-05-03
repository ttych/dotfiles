#!/bin/sh
# -*- mode: sh -*-

if [ $# -ne 1 ]; then
    cat <<EOF >&2
Usage is:
      $0  <sign-subkey-fingerprint>
EOF
fi

git config --global commit.gpgSign true
git config --global user.signingKey "$1!"
