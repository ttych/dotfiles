#!/bin/sh


DESKTOP_APP_DIR="$HOME/.local/share/applications"
DESKTOP_APP_ICON_DIR="$HOME/.local/share/applications/icons"

DEFAULT_ACTION=list


printf2()
{
    printf >&2 "$@"
}


desktop_app_list()
(
    desktop_app_list__usage="$0 list [-s] [pattern]"

    desktop_app_list__pattern='*'
    desktop_app_list__want_short=
    OPTIND=1
    while getopts :hxs opt; do
        case $opt in
            h) printf2 "%s\n" "$desktop_app_list__usage" ; return 0 ;;
            x) set -x ;;
            s) desktop_app_list__want_short=1
        esac
    done
    shift $(($OPTIND - 1))

    desktop_app_list__pattern="$1"

    cd "$DESKTOP_APP_DIR" 2>/dev/null || return 0
    IFS="
"
    for desktop_app_list__entry in $(find . -type f -name "${desktop_app_list__pattern}.desktop"); do
        desktop_app_list__filename="${desktop_app_list__entry##*/}"
        desktop_app_list__fullpath="${DESKTOP_APP_DIR}/${desktop_app_list__entry#./}"

        desktop_app_list__display_name="$desktop_app_list__fullpath"
        if [ -n "$desktop_app_list__want_short" ]; then
            desktop_app_list__display_name="$desktop_app_list__filename"
        fi
        printf "%s\n" "$desktop_app_list__display_name"
    done
)

desktop_app_add()
(
    desktop_app_add__usage="$0 add [-c categories] [-t type] <app_name> <command> <icon>
- (use icon from https://dashboardicons.com)
- categories in : AudioVideo, Audio, Video, Development, Education, Game, Graphics, Network, Office, Science, Settings, System, Utility
- type in : Application"

    desktop_app_add__default_categories=Utility;
    desktop_app_add__categories=
    desktop_app_add__type=Application
    OPTIND=1
    while getopts :hxc:t: opt; do
        case $opt in
            h) printf2 "%s\n" "$desktop_app_add__usage" ; return 0 ;;
            x) set -x ;;
            c) desktop_app_add__categories="${desktop_app_add__categories}${OPTARG%;};" ;;
            t) desktop_app_add__type="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    mkdir -p "$DESKTOP_APP_DIR" || return 1
    mkdir -p "$DESKTOP_APP_ICON_DIR" || return 1

    cd "$DESKTOP_APP_DIR"

    desktop_app_add__app="$1"
    desktop_app_add__command="$2"
    desktop_app_add__icon="$3"
    desktop_app_add__filepath="${desktop_app_add__app}.desktop"
    desktop_app_add__categories="${desktop_app_add__categories:-$desktop_app_add__default_categories}"

    if [ -z "$desktop_app_add__app" ]; then
        printf2 "%s\n" \
                "app_name should not be empty" \
                "$desktop_app_add__usage"
        return 1
    fi

    if [ -r "$desktop_app_add__filepath" ]; then
        printf2 "%s\n" \
                "the application $desktop_app_add__filepath already exists" \
                " at $(realpath $desktop_app_add__filepath)"
        return 1
    fi

    case $desktop_app_add__icon in
        http?://*)
            desktop_app_add__icon_local="${DESKTOP_APP_ICON_DIR}/${desktop_app_add__icon##*/}"
            if ! curl -sL -o "$desktop_app_add__icon_local" "$desktop_app_add__icon"; then
                echo "Error: Failed to download icon."
                return 1
            fi
            desktop_app_add__icon="$desktop_app_add__icon_local"
            ;;
        *) ;;
    esac

    cat > "$desktop_app_add__filepath" <<EOF
[Desktop Entry]
Version=1.0
Name=$desktop_app_add__app
Comment=$desktop_app_add__type $desktop_app_add__app
Exec=$desktop_app_add__command
Terminal=false
Type=$desktop_app_add__type
Icon=$desktop_app_add__icon
Categories=$desktop_app_add__categories
StartupNotify=true
EOF
    ## Extra:
    # MimeType=text/html;text/xml;application/xhtml_xml;

    chmod +x "$desktop_app_add__filepath"

    if which desktop-file-validate 2>/dev/null; then
        desktop-file-validate "$desktop_app_add__filepath"
    fi

    if which update-desktop-database 2>/dev/null; then
        update-desktop-database "$DESKTOP_APP_DIR"
    fi
)

desktop_app_edit()
(
    desktop_app_edit__usage="$0 edit [pattern]"

    OPTIND=1
    while getopts :hx opt; do
        case $opt in
            h) printf2 "%s\n" "$desktop_app_edit__usage" ; return 0 ;;
            x) set -x ;;
        esac
    done
    shift $(($OPTIND - 1))

    desktop_app_edit__pattern="$1"

    cd "$DESKTOP_APP_DIR" 2>/dev/null || return 0
    IFS="
"
    for desktop_app_edit__entry in $(find . -type f -name "${desktop_app_edit__pattern}.desktop"); do
        desktop_app_edit__fullpath="${DESKTOP_APP_DIR}/${desktop_app_edit__entry#./}"
        printf "# editing %s ...\n" "$desktop_app_edit__fullpath"
        eval $EDITOR "\"$desktop_app_edit__fullpath\""
    done

)

desktop_app_delete()
(
    desktop_app_delete__usage="$0 delete [-f] [app]"

    desktop_app_delete__want_force=
    OPTIND=1
    while getopts :hxf opt; do
        case $opt in
            h) printf2 "%s\n" "$desktop_app_delete__usage" ; return 0 ;;
            x) set -x ;;
            f) desktop_app_delete__want_force=1
        esac
    done
    shift $(($OPTIND - 1))

    desktop_app_delete__app="$1"
    if [ -z "$desktop_app_delete__app" ]; then
        printf2 "%s\n" "$desktop_app_delete__usage"
        return 1
    fi

    cd "$DESKTOP_APP_DIR" 2>/dev/null || return 0

    IFS="
"
    for desktop_app_delete__entry in $(find . -type f -name "$desktop_app_delete__app"); do
        echo $desktop_app_delete__entry
        desktop_app_delete__filename="${desktop_app_delete__entry##*/}"
        desktop_app_delete__fullpath="${DESKTOP_APP_DIR}/${desktop_app_delete__entry#./}"

        printf "# delete %s" "$desktop_app_delete__fullpath"
        if [ -z "$desktop_app_delete__want_force" ]; then
            printf " ? (y/N) "
            read answer
            case $anser in
                [Yy][Ee][Ss]|[Yy]) ;;
                *) continue ;;
            esac
        else
            printf "\n"
        fi
        rm "$desktop_app_delete__fullpath"
    done
)


######################################### main

usage()
{
    cat <<EOF
Usage:
  $0 [list|add|edit|delete] ...

Action:
    list    :
    add     :
    edit    :
    delete  :
EOF
    exit 1
}

OPTIND=1
while getopts :hx opt; do
    case $opt in
        h) usage ; exit 0 ;;
        x) set -x ;;
    esac
done
shift $(($OPTIND - 1))

ACTION=list
case "$1" in
    l|ls|list) ACTION=list ; shift ;;
    a|add) ACTION=add ; shift ;;
    e|edit) ACTION=edit ; shift ;;
    d|del|rm|delete) ACTION=delete ; shift ;;
esac
desktop_app_${ACTION} "$@"
