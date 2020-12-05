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

_git_info()
{
    _git_info__head_sha=
    _git_info__is_inside_work_tree=
    _git_info__is_inside_git_dir=
    _git_info__is_bare_repository=
    _git_info__git_dir=
    _git_info__git_name=
    _git_info__ignored=false
    _git_info__staged=false
    _git_info__changed=false
    _git_info__untracked=false
    _git_info__detached=false
    _git_info__branch=
    _git_info__action=
    _git_info__action_extended=
    _git_info__step=
    _git_info__total=

    _git_info__rev_parse="$(git rev-parse --absolute-git-dir --is-bare-repository --is-inside-git-dir --is-inside-work-tree --short HEAD 2>/dev/null)"
    _git_info__status=$?
    [ -z "$_git_info__rev_parse" ] && return 1

    _git_info__tmp="${_git_info__rev_parse}"
    _git_info__git_dir="${_git_info__tmp%%
*}"
    _git_info__tmp="${_git_info__tmp#$_git_info__git_dir?}"
    _git_info__is_bare_repository="${_git_info__tmp%%
*}"
    _git_info__tmp="${_git_info__tmp#$_git_info__is_bare_repository?}"
    _git_info__is_inside_git_dir="${_git_info__tmp%%
*}"
    _git_info__tmp="${_git_info__tmp#$_git_info__is_inside_git_dir?}"
    _git_info__is_inside_work_tree="${_git_info__tmp%%
*}"
    _git_info__tmp="${_git_info__tmp#$_git_info__is_inside_work_tree?}"
    _git_info__head_sha="${_git_info__tmp}"

    _git_info__git_name="${_git_info__git_dir}"
    [ "$_git_info__is_bare_repository" = "false" ] && _git_info__git_name="${_git_info__git_name%/.git}"
    _git_info__git_name="${_git_info__git_name##*/}"

    [ $_git_info__status -ne 0 ] && return 0

    if [ "$_git_info__is_inside_work_tree" = "true" ]; then
        $GIT check-ignore -q . && _git_info__ignored=true
        git diff --no-ext-diff --cached --quiet >/dev/null 2>/dev/null || _git_info__diff_staged=true
        git diff --no-ext-diff --quiet >/dev/null 2>/dev/null || _git_info__changed=true
        git ls-files --others --exclude-standard --directory --no-empty-directory --error-unmatch -- ':/*' >/dev/null 2>/dev/null && _git_info__untracked=true
    fi

    if [ -d "$_git_info__git_dir/rebase-merge" ]; then
        _git_info__action=rebase
        __git_eread "$_git_info__git_dir/rebase-merge/head-name" _git_info__branch
        __git_eread "$_git_info__git_dir/rebase-merge/msgnum" _git_info__step
        __git_eread "$_git_info__git_dir/rebase-merge/end" _git_info__total
    else
        if [ -d "$_git_info__git_dir/rebase-apply" ]; then
            __git_eread "$_git_info__git_dir/rebase-apply/next" _git_info__step
            __git_eread "$_git_info__git_dir/rebase-apply/last" _git_info__total
            if [ -f "$_git_info__git_dir/rebase-apply/rebasing" ]; then
                _git_info__action=rebase
                __git_eread "$_git_info__git_dir/rebase-apply/head-name" _git_info__branch
            elif [ -f "$_git_info__git_dir/rebase-apply/applying" ]; then
                _git_info__action=am
            else
                _git_info__action=am/rebase
            fi
        elif [ -f "$_git_info__git_dir/MERGE_HEAD" ]; then
            _git_info__action=merging
        elif test -f "$_git_info__git_dir/CHERRY_PICK_HEAD"; then
            _git_info__action=cherry-picking
        elif test -f "$_git_info__git_dir/REVERT_HEAD"; then
            _git_info__action=reverting
        elif __git_eread "$_git_info__git_dir/sequencer/todo" _git_info__todo; then
            case "$_git_info__todo" in
                p[\ \	]|pick[\ \	]*)
                    _git_info__action=cherry-picking ;;
                revert[\ \	]*)
                    _git_info__action=reverting ;;
            esac
        elif [ -f "$_git_info__git_dir/BISECT_LOG" ]; then
            _git_info__action=bisecting
        fi

        if [ -n "$_git_info__branch" ]; then
            :
        elif [ -h "$_git_info__git_dir/HEAD" ]; then
            # symlink symbolic ref
            _git_info__branch="$(git symbolic-ref HEAD 2>/dev/null)"
            _git_info__branch="${_git_info__branch#ref: refs/heads/}"
        else
            if ! __git_eread "$_git_info__git_dir/HEAD" _git_info__head; then
                return 0
            fi
            # is it a symbolic ref?
            _git_info__branch="${_git_info__head#ref: }"
            if [ "$_git_info__head" = "$_git_info__branch" ]; then
                _git_info__detached=true
                _git_info__branch="$(
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
                    _git_info__branch="$_git_info__head_sha"
            else
                _git_info__branch="${_git_info__branch#refs/heads/}"
            fi
        fi
    fi

    _git_info__action_ext="$_git_info__action"
    if [ -n "$_git_info__step" ] && [ -n "$_git_info__total" ]; then
        _git_info__action_ext="$_git_info__action_ext($_git_info__step/$_git_info__total)"
    fi
}

_git_prompt()
{
    _git_prompt=
    _git_prompt_alt=

    _git_prompt__cb="$1"
    _git_prompt__cok="$2"
    _git_prompt__cko="$3"
    _git_prompt__cr="$4"

    _git_info 2>/dev/null || return 1

    _git_prompt__git_name_prefix=
    if [ "$_git_info__is_bare_repository" = "true" ]; then
        _git_prompt__git_name_prefix=B/
    fi

    _git_prompt__status=
    [ "$_git_info__staged" = true ] &&
        _git_prompt__status="${_git_prompt__status}${_git_prompt__cok}+"
    [ "$_git_info__changed" = true ] &&
        _git_prompt__status="${_git_prompt__status}${_git_prompt__cko}*"
    [ "$_git_info__untracked" = true ] &&
        _git_prompt__status="${_git_prompt__status}${_git_prompt__cb}u"

    _git_prompt__branch="${_git_info__branch:-???}"
    case "$_git_prompt__branch" in
        master|"???") _git_prompt__branch="${_git_prompt__cko}${_git_prompt__branch}${_git_prompt__cb}" ;;
    esac
    if [ "$_git_info__detached" = true ]; then
        _git_prompt__branch="${_git_prompt__branch:-???}(d)"
    fi

    [ "$_git_info__ignored" = true ] && _git_info__ignored=ignored

    _git_prompt="${_git_prompt__cb}${_git_prompt__git_name_prefix}${_git_info__git_name}:${_git_prompt__branch}${_git_prompt__status:+:$_git_prompt__status}${_git_prompt__cr}"

    [ "$_git_info__ignored" = true ] &&
        _git_prompt_alt="${_git_prompt_alt:+$_git_prompt_alt }ignored"
    _git_prompt_alt="${_git_prompt_alt:+$_git_prompt_alt }${git_info__action_ext}"
    _git_prompt_alt="${_git_prompt__cb}${_git_prompt_alt}${_git_prompt__cr}"
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

_git_current_branch()
{
    _git_current_branch=$(git symbolic-ref HEAD 2> /dev/null || \
                                   git rev-parse --short HEAD 2> /dev/null)
    _git_current_branch="${_git_current_branch#refs/heads/}"
}

git_add()
{
    [ -n "$1" ] || return 1
    [ -f "$1" ] || return 1

    is_inside_git_work_tree || return 1
    $GIT add "$1"
}

_git_branch_current_name()
{
    _git_branch_current_name=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
}

_git_last_tag()
{
    _git_last_tag=$(git describe --abbrev=0 --tags 2>/dev/null)
}

_git_stable_state()
{
    test -z "$(git status --porcelain)"
}

_git_clone()
{
    _git_clone__opts=
    _git_clone__name_opts=
    _git_clone__suffix=
    OPTIND=1
    while getopts :123bs: _git_clone__opt; do
        case $_git_clone__opt in
            1|2|3) _git_clone__name_opts="$_git_clone__name_opts -$_git_clone__opt" ;;
            b) _git_clone__opts="$_git_clone__opts --bare"
               _git_clone__name_opts="$_git_clone__name_opts -$_git_clone__opt" ;;
            s) _git_clone__name_opts="$_git_clone__name_opts -$_git_clone__opt $OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$1" ] && return 1
    _git_clone__target="$2"
    if [ -z "$_git_clone__target" ]; then
        _git_url_to_name $_git_clone__name_opts "$1" || return 1
        _git_clone__target="$_git_url_to_name"
        [ -z "$_git_url_to_name" ] && return 1
        mkdir -p "$_git_url_to_name" || return 1
    fi
    git clone $_git_clone__opts "$1" $_git_clone__target
}
git_clone()
{
    _git_clone "$@"
}

_git_url_to_name()
{
    _git_url_to_name=

    _git_url_to_name__mode=1
    _git_url_to_name__sep=/
    _git_url_to_name__suffix=
    OPTIND=1
    while getopts :123bs: _git_url_to_name__opt; do
        case $_git_url_to_name__opt in
            1|2|3) _git_url_to_name__mode=$_git_url_to_name__opt ;;
            s) _git_url_to_name__sep="$OPTARG" ;;
            b) _git_url_to_name__suffix=.git ;;
        esac
    done
    shift $(($OPTIND - 1))

    [ -z "$1" ] && return 1

    _git_url_to_name__ifs="$IFS"
    IFS='@:/'
    set -- $1
    while [ $_git_url_to_name__mode -gt 0 ]; do
        eval _git_url_to_name="${_git_url_to_name:+$_git_url_to_name$_git_url_to_name__sep}"\${$(($# + 1 - $_git_url_to_name__mode))}
        _git_url_to_name__mode=$(($_git_url_to_name__mode - 1))
    done
    IFS="$_git_url_to_name__ifs"

    _git_url_to_name="${_git_url_to_name%.git}${_git_url_to_name__suffix}"

    _git_url_to_name="$(echo $_git_url_to_name | tr '[A-Z]' '[a-z]')"
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
_git_clone_or_update()
(
    _git_clone_or_update__source="$1"
    _git_clone_or_update__target="$2"

    [ -z "$_git_clone_or_update__source" ] && return 1
    [ -z "$_git_clone_or_update__target" ] && return 1

    cd "${_git_clone_or_update__target}" 2>/dev/null ||
        mkdir -p "${_git_clone_or_update__target}" ||
        return 1

    if [ -d "$_git_clone_or_update__target/.git" ]; then
        cd "${_git_clone_or_update__target}" &&
            git pull -q --no-rebase --ff-only
    else
        git clone -q "${_git_clone_or_update__source%.git}.git" \
                     "${_git_clone_or_update__target}"
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
