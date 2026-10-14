#!/bin/sh

if command -v getent >/dev/null 2>&1; then
  DEFAULT_SHELL=$(getent passwd "$LOGNAME" | cut -d ':' -f 7)
else
  DEFAULT_SHELL=$(grep "^$(id -un):" /etc/passwd | cut -d ':' -f 7)
fi

exec "$DEFAULT_SHELL" -l
