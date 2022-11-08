#!/bin/sh

set -x

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


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
        [YyOo]|[Yy][Ee][Ss]|[Oo][Uu][Ii]|"") return 0 ;;
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

    for conf_install__script in prepare.sh prepare_local.sh; do
        if [ -x "${conf_install__app}/${conf_install__script}" ]; then
            ( cd "${conf_install__app}" &&
                  INSTALL_DIR="$DOTFILES_INSTALL_DIR" "./$conf_install__script" )
        fi
    done

    if [ -r "${conf_install__app}/install.list" ]; then
        while read a b; do
            [ -z "$a" ] && continue
            [ -z "$b" ] && continue
            case "$a" in
                .) a="" ;;
                "#"*) continue ;;
                *) ;;
            esac

            # echo a : $a
            # echo b : $b

            a_rel=""
            b_tmp="$b"
            while true; do
                case $b_tmp in
                    ./*)
                        b_tmp="${b_tmp#./}"
                        ;;
                    ../*)
                        echo >&2 "unsupported .. in path"
                        return 2
                        ;;
                    */*)
                        b_tmp="${b_tmp#*/}"
                        a_rel="${a_rel}../"
                        ;;
                    *)
                        break
                        ;;
                esac
            done

            rm -f "${DOTFILES_INSTALL_DIR}/$b" && \
                ln -s "${a_rel}${DOTFILES_SOURCE_DIR}/${conf_install__app%/}/$a" "${DOTFILES_INSTALL_DIR}/$b"
        done  < "${conf_install__app}/install.list"
    fi

    for conf_install__script in install.sh install_local.sh; do
        if [ -x "${conf_install__app}/${conf_install__script}" ]; then
            ( cd "${conf_install__app}" &&
                  INSTALL_DIR="$DOTFILES_INSTALL_DIR" "./$conf_install__script" )
        fi
    done
}


DOTFILES_FORCE="${DOTFILES_FORCE:-false}"
DOTFILES_INSTALL_DIR="${DOTFILES_INSTALL_DIR:-$HOME}"
while getopts :hfp: opt; do
    case "$opt" in
        h) usage ; exit 0 ;;
        f) DOTFILES_FORCE=true ;;
        p) DOTFILES_INSTALL_DIR="$OPTARG" ;;
        *) usage ; exit 1 ;;
    esac
done
shift $(($OPTIND - 1))

# DOTFILES_SOURCE_DIR="${SCRIPT_PATH}"
DOTFILES_SOURCE_DIR="${SCRIPT_PATH#$DOTFILES_INSTALL_DIR/}"


export DOTFILES_INSTALL_DIR
export DOTFILES_SOURCE_DIR
export DOTFILES_FORCE

cd "${SCRIPT_RPATH}" || exit 1

if [ $# -eq 0 ]; then
    set -- */
else
    DOTFILES_FORCE=true
fi

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
