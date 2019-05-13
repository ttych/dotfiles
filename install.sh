#!/bin/sh

SCRIPT="${0}"
SCRIPT_NAME="${SCRIPT##*/}"
SCRIPT_PATH="${SCRIPT%$SCRIPT_NAME}"
case "$SCRIPT_PATH" in
    /*)  SCRIPT_RPATH="$SCRIPT_PATH" ;;
    ./*) SCRIPT_RPATH="${CUR_PATH}/${SCRIPT_PATH#./}" ;;
    *)   SCRIPT_RPATH="${CUR_PATH}/${SCRIPT_PATH}" ;;
esac
CUR_PATH=`pwd`

usage()
{
    echo "Usage is :"
    echo "  ${SCRIPT} -p install_path -f [module] [module 1] ..."
    echo ""
    echo "    -l install_path : default is \$HOME($HOME)"
    echo "    -f : force mode, no interactive"
    echo ""
    echo "    if no module specified, will install all"
}

user_accept()
{
    printf "%s ? (Y/n) " "$1"
    read user_accept__answer
    case $user_accept__answer in
        [YyOo]|[YyOo][EeUu][SsIi]) return 0 ;;
        *) return 1 ;;
    esac
}

conf_install()
{
    conf_install__app="$1"
    for conf_install__tpl in $(find "${conf_install__app}" -name '*.tpl' -print); do
        if $DOTFILES_FORCE || user_accept "$conf_install__app -> install template $conf_install__tpl"; then
            cp -f "${conf_install__tpl}" "${conf_install__tpl%.tpl}"
            $DOTFILES_FORCE || vi "${conf_install__tpl%.tpl}"
        fi
    done

    if [ -r "${conf_install__app}/install.list" ]; then
        while read a b; do
            [ -z "$a" ] && continue
            [ -z "$b" ] && continue
            case "$a" in
                .) a="" ;;
                "#"*) continue ;;
                *) a="/$a" ;;
            esac
            rm -f "${DOTFILES_INSTALL_DIR}/$b" && \
                ln -s "${DOTFILES_SOURCE_DIR}/${conf_install__app%/}$a" "${DOTFILES_INSTALL_DIR}/$b"
        done  < "${conf_install__app}/install.list"
    fi

    if [ -x "${conf_install__app}/install.sh" ]; then
        ( cd "${conf_install__app}" &&
              INSTALL_DIR="$DOTFILES_INSTALL_DIR" ./install.sh )
    fi
}


DOTFILES_FORCE="${DOTFILES_FORCE:-false}"
DOTFILES_INSTALL_DIR="${DOTFILES_INSTALL_DIR:-$HOME}"
while getopts :hp: opt; do
    case "$opt" in
        f) DOTFILES_FORCE=true ;;
        p) DOTFILES_INSTALL_DIR="$OPTARG" ;;
        h) usage ; exit 0 ;;
        *) usage ; exit 1 ;;
    esac
done
shift $(($OPTIND - 1))

DOTFILES_SOURCE_DIR="${SCRIPT_RPATH#$DOTFILES_INSTALL_DIR/}"

export DOTFILES_INSTALL_DIR
export DOTFILES_SOURCE_DIR
export DOTFILES_FORCE

cd "${SCRIPT_RPATH}" || exit 1

[ $# -eq 0 ] && set -- */
for app; do
    if [ ! -d "$app" ]; then
        echo >&2 "no conf for $app app"
        continue
    fi

    if ! $DOTFILES_FORCE; then
        user_accept "${app%/} -> install conf" || continue
    fi

    conf_install "$app" || \
        echo >&2 "failed to install conf for $app app"
done
