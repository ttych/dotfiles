#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`

URLS="
https://raw.githubusercontent.com/doitian/iy-go-to-char/master/iy-go-to-char.el
http://www.emacswiki.org/emacs/download/grep-a-lot.el
https://raw.githubusercontent.com/kljohann/turnip.el/master/turnip.el
http://www.emacswiki.org/emacs/download/dired-details.el
http://www.emacswiki.org/emacs/download/grep-a-lot.el
https://raw.githubusercontent.com/lewang/jump-char/master/jump-char.el
"

for url in $URLS; do
    lisp_lib="${url##*/}"
    echo "fetching ${lisp_lib} ..."
    wget -q -O "$SCRIPT_PATH/${lisp_lib}" "$url"
    emacs --batch --user=thomas --eval "(byte-compile-file \"${lisp_lib}\")" 2>/dev/null ||
        echo "failed to compule ${lisp_lib} !"
done

# bytecompile examples
# emacs -Q --batch --eval "(byte-recompile-directory '$SCRIPT_PATH')"
# emacs -Q --batch --eval '(byte-compile-file "your-elisp-file.el")'
