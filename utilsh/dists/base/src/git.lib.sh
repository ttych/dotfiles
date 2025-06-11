#!/bin/sh
# -*- mode: sh -*-

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


GIT=git


__git_eread ()
{
    test -r "$1" && IFS=$'\r\n' read "$2" <"$1"
}

__git_info()
{
    __git_info__head_sha=
    __git_info__is_inside_work_tree=
    __git_info__is_inside_git_dir=
    __git_info__is_bare_repository=
    __git_info__git_dir=
    __git_info__git_name=
    __git_info__ignored=false
    __git_info__staged=false
    __git_info__changed=false
    __git_info__untracked=false
    __git_info__detached=false
    __git_info__branch=
    __git_info__action=
    __git_info__action_extended=
    __git_info__step=
    __git_info__total=

    __git_info__rev_parse="$(git rev-parse --absolute-git-dir --is-bare-repository --is-inside-git-dir --is-inside-work-tree --short HEAD 2>/dev/null)"
    __git_info__status=$?
    [ -z "$__git_info__rev_parse" ] && return 1

    __git_info__tmp="${__git_info__rev_parse}"
    __git_info__git_dir="${__git_info__tmp%%
*}"
    __git_info__tmp="${__git_info__tmp#$__git_info__git_dir?}"
    __git_info__is_bare_repository="${__git_info__tmp%%
*}"
    __git_info__tmp="${__git_info__tmp#$__git_info__is_bare_repository?}"
    __git_info__is_inside_git_dir="${__git_info__tmp%%
*}"
    __git_info__tmp="${__git_info__tmp#$__git_info__is_inside_git_dir?}"
    __git_info__is_inside_work_tree="${__git_info__tmp%%
*}"
    __git_info__tmp="${__git_info__tmp#$__git_info__is_inside_work_tree?}"
    __git_info__head_sha="${__git_info__tmp}"

    __git_info__git_name="${__git_info__git_dir}"
    [ "$__git_info__is_bare_repository" = "false" ] && __git_info__git_name="${__git_info__git_name%/.git}"
    __git_info__git_name="${__git_info__git_name##*/}"

    [ $__git_info__status -ne 0 ] && return 0

    if [ "$__git_info__is_inside_work_tree" = "true" ]; then
        $GIT check-ignore -q . && __git_info__ignored=true
        git diff --no-ext-diff --cached --quiet >/dev/null 2>/dev/null || __git_info__diff_staged=true
        git diff --no-ext-diff --quiet >/dev/null 2>/dev/null || __git_info__changed=true
        git ls-files --others --exclude-standard --directory --no-empty-directory --error-unmatch -- ':/*' >/dev/null 2>/dev/null && __git_info__untracked=true
    fi

    if [ -d "$__git_info__git_dir/rebase-merge" ]; then
        __git_info__action=rebase
        __git_eread "$__git_info__git_dir/rebase-merge/head-name" __git_info__branch
        __git_eread "$__git_info__git_dir/rebase-merge/msgnum" __git_info__step
        __git_eread "$__git_info__git_dir/rebase-merge/end" __git_info__total
    else
        if [ -d "$__git_info__git_dir/rebase-apply" ]; then
            __git_eread "$__git_info__git_dir/rebase-apply/next" __git_info__step
            __git_eread "$__git_info__git_dir/rebase-apply/last" __git_info__total
            if [ -f "$__git_info__git_dir/rebase-apply/rebasing" ]; then
                __git_info__action=rebase
                __git_eread "$__git_info__git_dir/rebase-apply/head-name" __git_info__branch
            elif [ -f "$__git_info__git_dir/rebase-apply/applying" ]; then
                __git_info__action=am
            else
                __git_info__action=am/rebase
            fi
        elif [ -f "$__git_info__git_dir/MERGE_HEAD" ]; then
            __git_info__action=merging
        elif test -f "$__git_info__git_dir/CHERRY_PICK_HEAD"; then
            __git_info__action=cherry-picking
        elif test -f "$__git_info__git_dir/REVERT_HEAD"; then
            __git_info__action=reverting
        elif __git_eread "$__git_info__git_dir/sequencer/todo" __git_info__todo; then
            case "$__git_info__todo" in
                p[\ \	]|pick[\ \	]*)
                    __git_info__action=cherry-picking ;;
                revert[\ \	]*)
                    __git_info__action=reverting ;;
            esac
        elif [ -f "$__git_info__git_dir/BISECT_LOG" ]; then
            __git_info__action=bisecting
        fi

        if [ -n "$__git_info__branch" ]; then
            :
        elif [ -h "$__git_info__git_dir/HEAD" ]; then
            # symlink symbolic ref
            __git_info__branch="$(git symbolic-ref HEAD 2>/dev/null)"
            __git_info__branch="${__git_info__branch#ref: refs/heads/}"
        else
            if ! __git_eread "$__git_info__git_dir/HEAD" __git_info__head; then
                return 0
            fi
            # is it a symbolic ref?
            __git_info__branch="${__git_info__head#ref: }"
            if [ "$__git_info__head" = "$__git_info__branch" ]; then
                __git_info__detached=true
                __git_info__branch="$(
case "${_GIT_INFO_DESCRIBE_STYLE}" in
contains)
git describe --contains HEAD ;;
branch)
git describe --contains --all HEAD ;;
tag)
git describe --tags HEAD ;;
describe)
git describe HEAD ;;
*|default)
git describe --tags --exact-match HEAD ;;
esac 2>/dev/null
)" ||
                    __git_info__branch="$__git_info__head_sha"
            else
                __git_info__branch="${__git_info__branch#refs/heads/}"
            fi
        fi
    fi

    __git_info__action_ext="$__git_info__action"
    if [ -n "$__git_info__step" ] && [ -n "$__git_info__total" ]; then
        __git_info__action_ext="$__git_info__action_ext($__git_info__step/$__git_info__total)"
    fi
}

__git_prompt()
{
    __git_prompt=
    __git_prompt_alt=

    __git_prompt__cb="$1"
    __git_prompt__cok="$2"
    __git_prompt__cko="$3"
    __git_prompt__cr="$4"

    __git_info 2>/dev/null || return 1

    __git_prompt__git_name_prefix=
    if [ "$__git_info__is_bare_repository" = "true" ]; then
        __git_prompt__git_name_prefix=B/
    fi

    __git_prompt__status=
    [ "$__git_info__staged" = true ] &&
        __git_prompt__status="${__git_prompt__status}${__git_prompt__cok}+"
    [ "$__git_info__changed" = true ] &&
        __git_prompt__status="${__git_prompt__status}${__git_prompt__cko}*"
    [ "$__git_info__untracked" = true ] &&
        __git_prompt__status="${__git_prompt__status}${__git_prompt__cb}u"

    __git_prompt__branch="${__git_info__branch:-???}"
    case "$__git_prompt__branch" in
        master|"???") __git_prompt__branch="${__git_prompt__cko}${__git_prompt__branch}${__git_prompt__cb}" ;;
    esac
    if [ "$__git_info__detached" = true ]; then
        __git_prompt__branch="${__git_prompt__branch:-???}(d)"
    fi

    [ "$__git_info__ignored" = true ] && __git_info__ignored=ignored

    __git_prompt="${__git_prompt__cb}${__git_prompt__git_name_prefix}${__git_info__git_name}:${__git_prompt__branch}${__git_prompt__status:+:$__git_prompt__status}${__git_prompt__cr}"

    [ "$__git_info__ignored" = true ] &&
        __git_prompt_alt="${__git_prompt_alt:+$__git_prompt_alt }ignored"
    __git_prompt_alt="${__git_prompt_alt:+$__git_prompt_alt }${git_info__action_ext}"
    __git_prompt_alt="${__git_prompt__cb}${__git_prompt_alt}${__git_prompt__cr}"
}

is_inside_git()
{
    is_inside_git_work_tree
}

is_inside_git_work_tree()
{
    is_inside_git_work_tree=$($GIT rev-parse --is-inside-work-tree 2>/dev/null)
    [ "$is_inside_git_work_tree" = 'true' ]
}

__git_current_branch()
{
    __git_current_branch=$(git symbolic-ref HEAD 2> /dev/null || \
                                   git rev-parse --short HEAD 2> /dev/null)
    __git_current_branch="${__git_current_branch#refs/heads/}"
}

git_add()
{
    [ -n "$1" ] || return 1
    [ -f "$1" ] || return 1

    is_inside_git_work_tree || return 1
    $GIT add "$1"
}

__git_branch_current_name()
{
    __git_branch_current_name=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
}

__git_last_tag()
{
    __git_last_tag=$(git describe --abbrev=0 --tags 2>/dev/null)
}

__git_stable_state()
{
    test -z "$(git status --porcelain)"
}

__git_clone()
{
    __git_clone__opts=
    __git_clone__name_opts=
    __git_clone__suffix=
    OPTIND=1
    while getopts :123bs: __git_clone__opt; do
        case $__git_clone__opt in
            1|2|3) __git_clone__name_opts="$__git_clone__name_opts -$__git_clone__opt" ;;
            b) __git_clone__opts="$__git_clone__opts --bare"
               __git_clone__name_opts="$__git_clone__name_opts -$__git_clone__opt" ;;
            s) __git_clone__name_opts="$__git_clone__name_opts -$__git_clone__opt $OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$1" ] && return 1
    __git_clone__target="$2"
    if [ -z "$__git_clone__target" ]; then
        __git_url_to_name $__git_clone__name_opts "$1" || return 1
        __git_clone__target="$__git_url_to_name"
        [ -z "$__git_url_to_name" ] && return 1
        mkdir -p "$__git_url_to_name" || return 1
    fi
    git clone $__git_clone__opts "$1" $__git_clone__target
}
git_clone()
{
    __git_clone "$@"
}

__git_url_to_name()
{
    __git_url_to_name=

    __git_url_to_name__mode=1
    __git_url_to_name__sep=/
    __git_url_to_name__suffix=
    OPTIND=1
    while getopts :123bs: __git_url_to_name__opt; do
        case $__git_url_to_name__opt in
            1|2|3) __git_url_to_name__mode=$__git_url_to_name__opt ;;
            s) __git_url_to_name__sep="$OPTARG" ;;
            b) __git_url_to_name__suffix=.git ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$1" ] && return 1

    __git_url_to_name__ifs="$IFS"
    IFS='@:/'
    set -- $1
    while [ $__git_url_to_name__mode -gt 0 ]; do
        eval __git_url_to_name="${__git_url_to_name:+$__git_url_to_name$__git_url_to_name__sep}"\${$(($# + 1 - $__git_url_to_name__mode))}
        __git_url_to_name__mode=$(($__git_url_to_name__mode - 1))
    done
    IFS="$__git_url_to_name__ifs"

    __git_url_to_name="${__git_url_to_name%.git}${__git_url_to_name__suffix}"

    __git_url_to_name="$(echo $__git_url_to_name | tr '[A-Z]' '[a-z]')"
}

###

_semver_split()
{
    _semver_split="$1"
    _semver_split__major=${_semver_split%%.*}
    _semver_split=${_semver_split#$_semver_split__major}
    _semver_split=${_semver_split#.}
    _semver_split__minor=${_semver_split%%.*}
    _semver_split=${_semver_split#$_semver_split__major}
    _semver_split=${_semver_split#.}
    _semver_split__fix=${_semver_split}
    _semver_split__patch=${_semver_split__fix}
}


########## clone
__git_clone_or_update()
(
    __git_clone_or_update__source="$1"
    __git_clone_or_update__target="$2"

    [ -z "$__git_clone_or_update__source" ] && return 1
    [ -z "$__git_clone_or_update__target" ] && return 1

    cd "${__git_clone_or_update__target}" 2>/dev/null ||
        mkdir -p "${__git_clone_or_update__target}" ||
        return 1

    if [ -d "$__git_clone_or_update__target/.git" ]; then
        cd "${__git_clone_or_update__target}" &&
            git pull -q --no-rebase --ff-only
    else
        git clone -q "${__git_clone_or_update__source%.git}.git" \
                     "${__git_clone_or_update__target}"
    fi
)

is_git_tracked()
(
    [ -f "$1" ] || return 1

    is_git_tracked__file="${1##*/}"
    is_git_tracked__dir="${1%$is_git_tracked__file}"
    [ -n "$is_git_tracked__dir" ] && cd "$is_git_tracked__dir"

    git ls-files --error-unmatch "$is_git_tracked__file" >/dev/null 2>/dev/null
)



### main

case "$SCRIPT_NAME" in
    git_*) "$SCRIPT_NAME" "$@" ;;
esac
