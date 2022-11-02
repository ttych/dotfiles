#!/bin/sh
# -*- mode: shell-script -*-

SCRIPT="${0}"
SCRIPT_NAME="${SCRIPT##*/}"
SCRIPT_PATH="${SCRIPT%$SCRIPT_NAME}"
CUR_PATH=`pwd`
case "$SCRIPT_PATH" in
    /*)  SCRIPT_RPATH="$SCRIPT_PATH" ;;
    ./*) SCRIPT_RPATH="${CUR_PATH}/${SCRIPT_PATH#./}" ;;
    *)   SCRIPT_RPATH="${CUR_PATH}/${SCRIPT_PATH}" ;;
esac


usage()
{
    echo "Usage is :"
    echo "  ${SCRIPT} -p install_path"
    echo ""
    echo "    -p install_path : default is \$HOME($HOME)"
}


bootstrap_profiles()
{
    rm -f $INSTALL_DIR/.profile
    cat <<-EOF > $INSTALL_DIR/.profile
#!/bin/sh
# -*- mode: shell -*-
# bootstrap profile (autogenerated)

ESH_SH_INIT="\$0"

ESH_DIR='$SCRIPT_RPATH'
export ESH_DIR

. "\$ESH_DIR/base"

if [ -r "\$HOME/.profile.local" ]; then
    . "\$HOME/.profile.local"
fi
EOF

    ln -sf .profile $INSTALL_DIR/.zprofile
}

TO_LINKS='
zsh/zshrc .zshrc
bash/bashrc .bashrc
'

create_links()
{
    echo "$TO_LINKS" | while read create_links__src create_links__tgt; do
        if [ -z "$create_links__src" ] || [ ! -r "$create_links__src" ]; then
            continue
        fi
        [ -z "$create_links__tgt" ] && continue

        ln -sf "$SOURCE_DIR/$create_links__src" "$INSTALL_DIR/$create_links__tgt"
    done
}


FORCE=false
while getopts :hp: opt; do
    case "$opt" in
        p) INSTALL_DIR="$OPTARG" ;;
        h) usage ; exit 0 ;;
        *) usage ; exit 1 ;;
    esac
done
shift $(($OPTIND - 1))

INSTALL_DIR="${INSTALL_DIR:-$HOME}"
SOURCE_DIR="${SCRIPT_RPATH#$INSTALL_DIR/}"

export INSTALL_DIR SOURCE_DIR

cd ${SCRIPT_RPATH} || exit 1

bootstrap_profiles && create_links
