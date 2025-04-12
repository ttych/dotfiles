#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`

ORG_DIR="${ORG_DIR:-$HOME/org}"

eval $(printf "%b\n" "_ORG_RS=\"\036\"")


echo2()
{
    echo >&2 "$@"
}



######################################### emacs
emacs_edit()
{
    emacsclient -nw "$@"
}



######################################### emacs_org

emacs_org_usage()
{
    cat <<EOF
usage is:
  $SCRIPT_NAME ...
EOF
    exit 1
}

emacs_org_available()
{
    emacs_org_available="${_ORG_RS}"

    if ! [ -d "$emacs_org_base" ]; then
        return 0
    fi

    for emacs_org_note in "$emacs_org_base"/*"${1}"*.org; do
        [ -f "$emacs_org_note" ] && [ -r "$emacs_org_note" ] || continue

        emacs_org_available="${emacs_org_available}${emacs_org_note}${_ORG_RS}"
    done
}



emacs_org_prints()
(
    IFS="${_ORG_RS}"
    set -- $*
    for emacs_org_note; do
        [ -z "$emacs_org_note" ] && continue

        emacs_org_note_filename="${emacs_org_note##*/}"
        emacs_org_note_name="${emacs_org_note_filename%.org}"

        printf " %-12s %s\n" "${emacs_org_note_name}" "${emacs_org_note}"
    done

)

emacs_org_list()
{
    emacs_org_list=
    if [ $# -eq 0 ]; then
        emacs_org_available
        emacs_org_list="$emacs_org_available"
    else
        for emacs_org_note_r; do
            emacs_org_available "${emacs_org_note_r}"
            emacs_org_list="${emacs_org_list}${emacs_org_available}"
        done
    fi

    emacs_org_prints "$emacs_org_list"
}

emacs_org_edit()
{
    for emacs_org_note_r; do
        [ -z "$emacs_org_note_r" ] && continue

        emacs_org_edit_count=0
        emacs_org_available "$emacs_org_note_r"
        emacs_org_edit_all="${emacs_org_available#$_ORG_RS}"
        while [ -n "$emacs_org_edit_all" ]; do
            emacs_org_note="${emacs_org_edit_all%%$_ORG_RS*}"
            emacs_org_edit_all="${emacs_org_edit_all#$emacs_org_note}"
            emacs_org_edit_all="${emacs_org_edit_all#$_ORG_RS}"
            [ -z "$emacs_org_note" ] && continue

            emacs_org_edit_count=$(( $emacs_org_note_edit_count + 1 ))
            emacs_edit "$emacs_org_note"
        done

        if [ $emacs_org_edit_count -eq 0 ]; then
            emacs_org_edit_filename="${emacs_org_note_r%.org}.org"
            # printf "%s ? (y/n) " "create $emacs_org_edit_filename"
            # read emacs_org_answer
            # case $emacs_org_answer in
                # [Yy]|[Yy][Ee][Ss])
                    emacs_edit "$emacs_org_base"/"$emacs_org_edit_filename"
                    # ;;
                # *) ;;
            # esac
        fi
    done
}

emacs_org()
{
    OPTIND=1
    while getopts :h opt; do
        case $opt in
            h) emacs_org_usage
               return 0
               ;;
        esac
    done
    shift $(($OPTIND - 1))

    case "$1" in
        "") emacs_org_list ;;
        list|ls) shift; emacs_org_list "$@" ;;
        edit) shift; emacs_org_edit "$@" ;;
        *) emacs_org_edit "$@" ;;
    esac
}



######################################### onotes
ONOTES_DIR="${ONOTES_DIR:-$ORG_DIR/notes}"

onotes()
{
    emacs_org_base="$ONOTES_DIR" emacs_org "$@"
}

######################################### otasks
OTASKS_DIR="${OTASKS_DIR:-$ORG_DIR/tasks}"

otasks()
{
    emacs_org_base="$OTASKS_DIR" emacs_org "$@"
}



######################################### main

case "$SCRIPT_NAME" in
    onotes|onotes.sh)
        onotes "$@" ;;
    otasks|otasks.sh)
        otasks "$@" ;;
    *) echo2 "#."
       exit 0
       ;;
esac
