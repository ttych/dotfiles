#!/bin/sh
# -*- mode: sh -*-

echo "cmdline:" "$0" "$@"
index=0
while [ $index -le $# ]; do
    eval echo "'\$$index:'" "\"\$$index\""
    index=$(($index + 1))
done
echo '$#:' "$#"
echo '$@:' "$@"
echo '$*:' "$*"
