#!/bin/sh

mkdir -p "$INSTALL_DIR/.ssh"
chmod 0700 "$INSTALL_DIR/.ssh"
mkdir -p "$INSTALL_DIR/.ssh/keys"
chmod 0700 "$INSTALL_DIR/.ssh/keys"
