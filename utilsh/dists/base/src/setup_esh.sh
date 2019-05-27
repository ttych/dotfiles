#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_PATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH="${SCRIPT_PATH%/}"
SCRIPT_PATH=`cd "${SCRIPT_PATH:-$PWD}" && pwd -P`

ESH_DIR="${1:-$HOME/.esh}"
GIT_REPOS="https://github.com/ttych/esh.git
           https://gitlab.com/ttych/esh.git"


clone()
(
    for git_repo in $GIT_REPOS; do
        if [ -d "$ESH_DIR" ] && [ -d "$ESH_DIR/.git" ]; then
            current_repo=`git remote get-url --all origin`
            if [ "$current_repo" != "$git_repo" ]; then
                git remote remove origin || continue
                git remote add origin "$git_repo" || continue
            fi
            return 0
        else
            git clone "$git_repo" "$ESH_DIR" && return 0
        fi
    done
    return 1
)

update()
(
    cd "$ESH_DIR" || return 1
    update__branch="$(git branch)"
    git pull origin "${update__branch:-master}" --no-rebase --ff-only
)

install()
{
    cd "$ESH_DIR" &&
        [ -r install.sh ] &&
        ESH_UTILS_DIR="${SCRIPT_PATH%/*}" ./install.sh
}

clone && update && install
