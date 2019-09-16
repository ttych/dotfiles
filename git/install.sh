#!/bin/sh
# -*- mode: sh -*-


which git >/dev/null 2>&1 || {
    echo "No git command found ! Aborting git configuration"
    exit 1
}


GIT_VERSION=`git --version`
GIT_VERSION=${GIT_VERSION##* }


# git config --local --list
# git config --global --list
# git config --system --list


# config
[ -n "$USER_NAME" ] &&
    git config --global user.name "$USER_NAME"
[ -n "$USER_MAIL" ] &&
    git config --global user.email "$USER_MAIL"


# color output
git config --global color.ui true

# editor set to emacs if available
for editor in e et emacs vim vi; do
    if command -v $editor >/dev/null 2>/dev/null; then
        git config --global core.editor $editor
        break
    fi
done

git config --global alias.ec 'config --global -e'

# algorithm : patience / histogram / <default>
git config --global diff.algorithm histogram

# pager
git config --global core.pager 'less -RFX'

# newline
## To configure line endings correctly on Linux/Mac:
##git config --global core.autocrlf input
## To configure line endings correctly on Windows:
##git config --global core.autocrlf true

# ReReRe : Reuse Recorded Resolution
git config --global rerere.enabled true

# Rebase + autosquash
## causes git rebase -i to parse magic comments created
## by git commit --squash=some-hash and git commit --fixup=some-hash
## and reorder the commit list before presenting it for further editing
git config --global rebase.autosquash true

# garbage collection cycle
## 90 days reachable / gc.reflogExpire
## 30 days unreachable / gc.reflogExpireUnreachable

# merge
git config --global merge.conflictStyle diff3


# status
git config --global alias.st status
git config --global alias.s "status --short --branch"
#git config --global advice.statusuoption false

# clone
git config --global alias.cl clone

# checkout
git config --global alias.co checkout
git config --global alias.com 'checkout master'
git config --global alias.cor 'checkout --track'

# branch
git config --global alias.br branch
## git show-branch <br1> <br2>
git config --global alias.merged 'branch --merged'
git config --global alias.unmerged 'branch --no-merge'

# commit
git config --global alias.ci commit
git config --global alias.cam "commit --all -m"

# fetch
git config --global alias.fe fetch

# diff
git config --global alias.di diff
git config --global alias.diffcached 'diff --cached'
git config --global alias.diffc 'diff --cached'
git config --global alias.diffstaged 'diff --staged'
git config --global alias.diffs 'diff --staged'
git config --global alias.diffword 'diff --word-diff'
git config --global alias.diffw 'diff --word-diff'
git config --global alias.difftext 'diff --word-diff --unified=10'
git config --global alias.difft 'diff --word-diff --unified=10'

# log
git config --global alias.ll 'log --pretty=fuller'
git config --global alias.l1 'log --oneline --decorate --abbrev-commit --all'
git config --global alias.l1g 'log --oneline --decorate --abbrev-commit --all --graph'
git config --global alias.lg "log --pretty='%C(yellow)%h%Creset %C(red)%d%Creset %s %Cgreen(%cr)%Creset %C(cyan)[%an]%Creset' --graph"
git config --global alias.la g "lg --all"
## path from branch A to branch B (ancestor A / decendent B)
## git lg --ancestry-path B..A

# show
git config --global alias.so "show --pretty='parent %C(yellow)%p%Creset commit %C(yellow)%h%Creset%Cred%d%Creset%n%n%w(72,2,2)%s%n%n%w(72,0,0)%C(cyan)%an%Creset %Cgreen%ar%Creset'"

# plumb
git config --global alias.plumb 'cat-file -p'

# merge
git config --global alias.mergeto '!git checkout $1 && git merge @{-1}'
git config --global alias.truemerge "merge --no-ff"
git config --global alias.ffmerge "merge --ff-only"

# pull
git config --global alias.ffpull "pull --no-rebase --ff-only"
git config --global alias.repull "pull --rebase"
git config --global alias.up '!git pull --rebase $@ && git submodule update --init --recursive'

# push
git config --global alias.pushall "push --recurse-submodules=on-demand"
git config --global alias.push2all '!f() { for remote in `git remote`; do git push $remote "$@"; done }; f'
# push : nothing | matching | simple | current
git config --global push.default current
git config --global alias.pushf 'push --force-with-lease'

# reset
## --hard / update : HEAD + index + workspace (destructive)
## --mixed (default) / update : HEAD + index
## --soft / update : HEAD
git config --global alias.unstage 'reset HEAD'
git config --global alias.resetto '!f() { f=$(git symbolic-ref HEAD 2>/dev/null); f="origin/${f#refs/heads/}" ; git reset --hard "${1:-$f}"; }; f'
git config --global alias.resetfile '!f() { git reset @~ "$@" && git commit --amend --no-edit }; f'

# whitespace
## indent-with-non-tab, tab-in-indent : tab & indentation
## trailing-space : remove trailing space
## space-before-tab : remove space before tab
## cr-at-eol : CR at end of line
git config --global core.whitespace 'blank-at-eol,blank-at-eof,trailing-space,space-before-tab,cr-at-eol'
git config --global alias.fixwhitespace '!f() {git rebase HEAD~${1:-1} --whitespace=fix}; f'

# stash = index + workspace (default)
git config --global alias.saveall 'stash save --include-untracked "SAVE ALL"'
git config --global alias.saveun 'stash save --keep-index --include-untracked "SAVE UNTRACKED"'

# references
git config --global alias.references 'show-ref'
git config --global alias.refs 'show-ref'
git config --global alias.logref "log --walk-reflogs"

# fsck
git config --global alias.dangling 'fsck --dangling --no-reflogs'  # --no-progress
git config --global alias.unreachable 'fsck --unreachable --no-reflogs'  # --no-progress

# cherry-pick
git config --global alias.append '!git cherry-pick $(git merge-base HEAD $1)..$1'

# contributors
git config --global alias.contributors 'shortlog -s -n'



# branch-diff
git config --global alias.branchdiff '!f() { br1=HEAD; [ $# -gt 1 ] && br1="$1" && shift; br2="$1" ; git lg --cherry-mark --left-right --no-merges "$br2"..."$br1"; }; f'
git config --global alias.branchdiffx '!f() { br1=HEAD; [ $# -gt 1 ] && br1="$1" && shift; br2="$1" ; git lg --cherry-pick --left-right --no-merges "$br2"..."$br1"; }; f'
git config --global alias.branchdiffmissing '!f() { br1=HEAD; [ $# -gt 1 ] && br1="$1" && shift; br2="$1" ; git lg --cherry-pick --left-only --no-merges "$br2"..."$br1"; }; f'

# unmerged commit
#git config --global alias.unmergedc '!f() { brs="$1" ; [ -z "$brs" ] && brs=$(git unmerged); for br in $brs; do echo "# $br"; git lg HEAD..$br; done; }; f'
git config --global alias.unmergedc '!f() { brs="$1" ; [ -z "$brs" ] && brs=$(git unmerged); for br in $brs; do echo "# $br"; git branchdiffmissing $br; done; }; f'

# merge conflict
git config --global alias.mergecontext 'lg --merge --name-only'
git config --global alias.diff3wm 'checkout --conflict=diff3'

# reflog navigation
git config --global alias.undo '!f() { git reset --hard $(git rev-parse --abbrev-ref HEAD)@{${1:-1}}; }; f'



# rebase
git config --global alias.irebase '!f() { f=$(git symbolic-ref HEAD 2>/dev/null); f="origin/${f#refs/heads/}" ; git rebase --interactive "${1:-$f}"; }; f'

# grep
git config --global alias.g "grep --break --heading --line-number"



# git pull --rebase
WANT_REBASE=false
if $WANT_REBASE; then
    # git >= 1.8.5
    version_tmp=$(printf "%s\n" $GIT_VERSION 1.8.5 | sort -r | head -1)
    if [ "$version_tmp" = "$GIT_VERSION" ]; then
        git config --global pull.rebase preserve
    else
        # git >= 1.7.9
        version_tmp=$(printf "%s\n" $GIT_VERSION 1.7.9 | sort -r | head -1)
        if [ "$version_tmp" = "$GIT_VERSION" ]; then
            git config --global pull.rebase true
        else
            # git < 1.7.9
            git config --global branch.autosetuprebase always
        fi
    fi
fi



# git :
## log
## show-branch | --topic master br1 br2
## branch | --merged | --no-merged
## branch --all
## git show-ref --heads
## lg --cherry-mark --left-right --no-merges master...feature
## lg --cherry-pick --left-right --no-merges master...feature
## lg --cherry master...feature
## lg --merges feature..master
## log --merge --name-only
## log --follow code_file.rb
## log -Sstring
## log -S".*pattern.*" --pickaxe-regex
## log -G".*pattern.*"
## log -L:function_name:code_file.rb
## fsck --dangling --no-progress
## fsck --unreachable --no-progress
## cherry-pick
## merge-base HEAD MERGE_HEAD
## checkout --conflict=diff3 calculator.c
## rebase -i --root --autosquash (autosquashing / fixup! <commit_msg>)
## log --grep=C --walk-reflogs
## bisect start ; bisect bad HEAD ; bisect good HEAD~4 ; bisect good | bad ; bisect reset
## bisect start HEAD HEAD~4 ; git bisect run <make>



# dot Notation
## diff
## Two Dot Notation / Dot Dot Notation / A..B => range of commit between ref A and ref B
## log
## Two Dot Notation / Dot Dot Notation / A..B => commit reachable from ref B but not from ref A
## Three Dot Notation / Dot Dot Dot Notation / A...B => commit reachable for either of ref A or B but not from both


# browse
git config --global alias.browse '!f() { url=$(git config remote.origin.url | sed -e "s/^git@\([a-z.-]*\):\(.*\)$/https:\/\/\1\/\2/") ; xdg-open ${url}; }; f'
