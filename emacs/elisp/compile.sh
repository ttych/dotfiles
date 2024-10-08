#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`

for module; do
    case "$module" in
        *.el)
            # emacs -Q --batch  --eval "(byte-compile-file \"${lisp_module}\")"  # 2>/dev/null
            emacs -Q --batch \
                  --eval "(setq byte-compile-warnings nil)" \
                  --eval "(setq warning-minimum-level :error)" \
                  --eval "(setq warning-minimum-log-level :error)" \
                  -f batch-byte-compile "$module" 2>/dev/null
            ;;
        *)
            echo >&2 "$module could not compiled, expecting .el file"
            ;;
    esac
done
