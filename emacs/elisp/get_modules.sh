#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`

MODULE_URLS="
https://raw.githubusercontent.com/doitian/iy-go-to-char/refs/heads/master/iy-go-to-char.el
https://raw.githubusercontent.com/winterTTr/ace-jump-mode/refs/heads/master/ace-jump-mode.el
"

for url in $MODULE_URLS; do
    lisp_module="${url##*/}"
    echo -n "fetching ${lisp_module} ... "
    if wget -q -O "$SCRIPT_PATH/${lisp_module}" "$url"; then
        echo "OK"
        echo -n "compile ${lisp_module} ... "
        "$SCRIPT_PATH/compile.sh" "$SCRIPT_PATH/${lisp_module}" &&
            echo "OK" || echo "FAILED !"
    else
        echo "FAILED"
    fi
done

# bytecompile examples
# emacs -Q --batch --eval "(byte-recompile-directory '$SCRIPT_PATH')"
# emacs -Q --batch --eval '(byte-compile-file "your-elisp-file.el")'
