#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`



######################################### clean
python_clean_cache()
{
    for python_clean_cache__d in "${@:-$PWD}"; do
        [ -d "$python_clean_cache__d" ] || continue

        echo "cleaning python cache in $python_clean_cache__d ..."
        find "$python_clean_cache__d" \( -name '__pycache__' -o -name '*.pyc' -o -name '.mypy_cache' \) -exec rm -Rf {} \; 2>/dev/null
    done
    return 0
}
clean_python_cache()
{
    python_clean_cache "$@"
}



######################################### main
case "$SCRIPT_NAME" in
    clean_python_cache|python_clean_cache)
        "$SCRIPT_NAME" "$@"
        ;;
esac
