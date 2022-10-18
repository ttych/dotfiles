#!/bin/sh
# -*- mode: sh -*-


# A good commit
# - Atomic
#   self-contained: sementically related changes should not be splitted across commits
#   coherent: all change in 1 commit should be semantically related
# - Consistent
#   no compilation errors
#   no broken tests
#   -> should not introduction quality regressions
# - Incremental
#   ordered: should be ordered deliberately (not arbitrary)
#   explanatory: should be a trail of the programmer's thought process
# - Documented
#   a short summary: a short one-sentence summary
#   detailed description: more details


which git >/dev/null 2>&1 || {
    echo "No git command found ! Aborting git configuration"
    exit 1
}


GIT_VERSION=`git --version`
GIT_VERSION=${GIT_VERSION##* }


# commit
#  - parent (reference)
#  - the state of the files (snapshot)
# snapshot
#  - directory (tree)
#  - file (blob)
# reference
#  - tags (associated to a specific commit)
#  - branches (latest commit in a history line)
#  - HEAD (special reference to the snapshot of the current working directory)


# git config --local --list
# git config --global --list
# git config --system --list

### config
git_current_user_name=`git config --global --get user.name`
if [ -z "$git_current_user_name" ]; then
    [ -n "$USER_NAME" ] &&
        git config --global user.name "$USER_NAME"
fi
git_current_user_mail=`git config --global --get user.mail`
if [ -z "$git_current_user_mail" ]; then
    [ -n "$USER_MAIL" ] &&
        git config --global user.email "$USER_MAIL"
fi

git config --global alias.confe 'config --global -e'
git config --global alias.configl 'config --list --show-origin'
git config --global alias.confl 'config --list --show-origin'

### default Branch
git config --global init.defaultBranch master
# git config --global init.defaultBranch main

### global gitignore
if [ -r "$HOME/.gitignore_global" ]; then
    git config --global core.excludesfile "$HOME/.gitignore_global"
fi

### autocorrect command
# git config --global help.autocorrect 1  # number of 1/10 seconds before doing

### credentials
# git config --global credential.helper cache

### color output
git config --global color.ui auto
git config --global color.branch auto
git config --global color.diff auto
git config --global color.interactive auto
git config --global color.status auto
# blue foreground / black background / bold text
# git config --global color.diff.meta "blue black bold"
# text properties: bold, dim, ul (underline), blink, reverse

# editor set to emacs if available
for editor in e et emacs vim vi; do
    if command -v $editor >/dev/null 2>/dev/null; then
        git config --global core.editor $editor
        break
    fi
done

# autocorrect
# git config --global help.autocorrect 20

# commit template
if [ -r "$HOME/.gitmessage.txt" ]; then
    git config --global commit.template "$HOME/.gitmessage.txt"
fi

# algorithm : patience / histogram / <default>
git config --global diff.algorithm histogram

# pager
git config --global core.pager 'less -RFX'
# git config --global core.pager ''

# newline
## (linux) to convert CRLF to LF on commit but not the other way around
git config --global core.autocrlf input
## (windows) converts LF endings into CRLF when you check-out code
# git config --global core.autocrlf true
## (windows-only)
# git config --global core.autocrlf false

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

## merge
git config --global merge.conflictStyle diff3

## init
git config --global alias.i init

## status
git config --global alias.st status
git config --global alias.s "status --short --branch"
# git config --global advice.statusuoption false

## clone
git config --global alias.cl clone

## checkout
git config --global alias.co checkout
git config --global alias.cob 'checkout -b'
git config --global alias.com 'checkout master'
git config --global alias.cor 'checkout --track'

## branch
git config --global alias.br branch
git config --global alias.brv 'branch -v'
git config --global alias.brvv 'branch -vv'
git config --global alias.bra 'branch --all -v'
git config --global alias.brav 'branch --all -vv'
git config --global alias.merged 'branch --merged'
git config --global alias.unmerged 'branch --no-merged'
# git config --global alias.cleanmerged '!f() { git branch --merged ${1:-master} | grep -v " ${1:-master}$" | xargs git branch -d; }; f
git config --global alias.cleanmerged '!f() { for br in $(git branch --merged ${1:-master} | grep -v " ${1:-master}$"); do git branch -d "$br"; done; }; f'
## git show-branch <br1> <br2>
git config --global alias.bru 'branch -u'

## add
git config --global alias.a add

# commit
git config --global alias.ci commit
git config --global alias.cim "commit -m"
git config --global alias.cam "commit --all -m"
git config --global alias.amend 'commit --amend'
git config --global alias.amendh 'commit --amend -C HEAD'
git config --global alias.amendn 'commit --amend --no-edit'
git config --global alias.fixup 'commit --fixup'
git config --global alias.squash 'commit --squash'

### rebase
git config --global alias.rb 'rebase'
git config --global alias.rbi 'rebase -i'
git config --global alias.rbif 'rebase -i --fork-point'
git config --global alias.irb 'rebase -i'
git config --global alias.irbf 'rebase -i --fork-point'
# equivalent to
# git rebase -i $(git merge-base --fork-point <branch>)
git config --global alias.rbpreview '!f() { git log --reverse --pretty=format:"%h %s" ${1}..HEAD; }; f'

### rm
git config --global alias.untrack 'rm --cache --'

### remote
git config --global alias.rem 'remote -v'
git config --global alias.remshow 'remote -v show'
git config --global alias.remref 'ls-remote'

### fetch
git config --global alias.fe fetch

### diff
git config --global alias.di diff
git config --global alias.dc 'diff --check'
git config --global alias.diffcached 'diff --cached'
git config --global alias.diffc 'diff --cached'
git config --global alias.ds 'diff --staged'
git config --global alias.diffstaged 'diff --staged'
git config --global alias.diffs 'diff --staged'
git config --global alias.diffword 'diff --word-diff'
git config --global alias.diffw 'diff --word-diff'
git config --global alias.difftext 'diff --word-diff --unified=10'
git config --global alias.difft 'diff --word-diff --unified=10'

### log
git config --global alias.last 'log -1 HEAD'
git config --global alias.lf  'log --pretty=fuller'
git config --global alias.lp  'log --patch'
git config --global alias.lg  'log --decorate --graph'
git config --global alias.la  'log --decorate --all'
git config --global alias.lag 'log --decorate --all --graph'
git config --global alias.l   'log --oneline --decorate --abbrev-commit'
git config --global alias.ll  'log --oneline --decorate --abbrev-commit --graph'
git config --global alias.lla 'log --oneline --decorate --abbrev-commit --graph --all'
# git config --global alias.l1 "log --pretty='%C(yellow)%h %Cred%cr %Cblue(%an)%C(white)%d%Creset %s'"
git config --global alias.l1 "log --pretty='%C(yellow)%h%Creset %C(red)%d%Creset %s %Cgreen(%cr)%Creset %C(cyan)[%an]%Creset'"
## path from branch A to branch B (ancestor A / decendent B)
## git lg --ancestry-path B..A
git config --global alias.lo   'log --oneline --decorate'
git config --global alias.lol  'log --oneline --decorate --graph'
git config --global alias.lola 'log --oneline --decorate --graph --all'

### show-branch
git config --global alias.sb 'show-branch'
git config --global alias.sbt '!f() { bt="$1"; [ $# -gt 0 ] && shift ; [ -z "$bt" ] && bt=$(git rev-parse --abbrev-ref HEAD); git show-branch --topic $bt "$@"; }; f'

### show
git config --global alias.so "show --pretty='parent %C(yellow)%p%Creset commit %C(yellow)%h%Creset%Cred%d%Creset%n%n%w(72,2,2)%s%n%n%w(72,0,0)%C(cyan)%an%Creset %Cgreen%ar%Creset'"

### plumb
git config --global alias.plumb 'cat-file -p'

### merge
git config --global alias.mergeto '!git checkout $1 && git merge @{-1}'
git config --global alias.truemerge "merge --no-ff"
git config --global alias.ffmerge "merge --ff-only"

### pull
git config --global alias.ffpull "pull --no-rebase --ff-only"
git config --global alias.repull "pull --rebase"
git config --global alias.up '!git pull --rebase $@ && git submodule update --init --recursive'

### push
git config --global alias.push2all '!f() { for remote in `git remote`; do git push $remote "$@"; done }; f'
# push : nothing | matching | simple | current
git config --global push.default current
git config --global alias.pushf 'push --force-with-lease'
git config --global alias.rm-remote-ref '!f() { [ $# -ge 2 ] && rem="$1" && shift ; git push "${rem:-origin}" --delete "$@"; }; f'
# alternativ is: git push <origin> :refs/tags/<v1.4-lw>

### submodules
git config --global submodule.recurse true
git config --global diff.submodule log
git config --global status.submodulesummary 1
git config --global push.recurseSubmodules check

git config --global alias.sinit 'submodule update --init --recursive'
git config --global alias.sco 'checkout --recurse-submodules'
git config --global alias.sdiff '!'"git diff && git submodule foreach 'git diff'"
git config --global alias.spush 'push --recurse-submodules=on-demand'
git config --global alias.supdate 'submodule update --remote --merge'

### tag
git config --global alias.rtag 'describe --exact-match --tags'

### restore
git config --global alias.unstage- 'restore --staged --'
git config --global alias.unmodify 'restore --'

### switch
git config --global alias.sw 'switch'
git config --global alias.swc 'switch -c'
git config --global alias.swb 'switch -'

### reset
## --hard / update : HEAD + index + workspace (destructive)
## --mixed (default) / update : HEAD + index
## --soft / update : HEAD
git config --global alias.unstage 'reset HEAD --'
git config --global alias.resetto '!f() { branch=$(git rev-parse --abbrev-ref HEAD); git reset --hard "${1:-$branch}"; }; f'
git config --global alias.resetfile '!f() { git reset @~ "$@" && git commit --amend --no-edit }; f'

### whitespace
## 6 whitespace issues
## 3 defaults: blank-at-eol,blank-at-eof,space-before-tab
## additional: cr-at-eol,tab-in-indent,indent-with-non-tab
## trailing-space = both blank-at-eol + blank-at-eof
git config --global core.whitespace 'trailing-space,space-before-tab'
git config --global alias.cleanwhitespace '!f() {git rebase HEAD~${1:-1} --whitespace=fix}; f'

# stash = index + workspace (default)
git config --global alias.saveall 'stash save --include-untracked "SAVE ALL"'
git config --global alias.savewip 'stash save --keep-index --include-untracked "SAVE WIP"'
git config --global alias.savefull 'stash save --include-untracked --all "SAVE FULL"'
git config --global alias.st2br 'stash branch'

# references
git config --global alias.references 'show-ref'
git config --global alias.refs 'show-ref'
git config --global alias.logreferences "log --walk-reflogs"
git config --global alias.logref "log --walk-reflogs"
git config --global alias.reflogs 'log --walk-reflogs'

# fsck
git config --global alias.dangling 'fsck --dangling --no-reflogs'  # --no-progress
git config --global alias.unreachable 'fsck --unreachable --no-reflogs'  # --no-progress

# cherry-pick
git config --global alias.cherry-picks '!git cherry-pick $(git merge-base HEAD $1)..$1'
git config --global alias.append '!git cherry-pick $(git merge-base HEAD $1)..$1'

# contributors
git config --global alias.contribs 'shortlog -s -n'
git config --global alias.contributors 'shortlog -s -n'

# branch-diff
git config --global alias.branchdiff '!f() { br1=HEAD; [ $# -gt 1 ] && br1="$1" && shift; br2="$1" ; git lg --cherry-mark --left-right --no-merges "$br2"..."$br1"; }; f'
git config --global alias.brdiff '!git branchdiff'
git config --global alias.branchdiffp '!f() { br1=HEAD; [ $# -gt 1 ] && br1="$1" && shift; br2="$1" ; git lg --cherry-pick --left-right --no-merges "$br2"..."$br1"; }; f'
git config --global alias.brdiffp '!git branchdiffp'
git config --global alias.missingto '!f() { [ $# -lt 2 ] && set -- HEAD "$1"; git lg --cherry-pick --left-only --no-merges "$1"..."$2"; }; f'
git config --global alias.missto '!git missingto'
git config --global alias.missingfrom '!f() { [ $# -lt 2 ] && set -- HEAD "$1"; git lg --cherry "$1"..."$2"; }; f'
git config --global alias.missfrom '!git missingfrom'

# tracking changes
git config --global alias.commitsonfile '!git lg --follow'
git config --global alias.commitsonstring '!f() { git lg -G"$1"; }; f'

# unmerged commit
#git config --global alias.unmergedc '!f() { brs="$1" ; [ -z "$brs" ] && brs=$(git unmerged); for br in $brs; do echo "# $br"; git lg HEAD..$br; done; }; f'
git config --global alias.unmergedc '!f() { brs="$1" ; [ -z "$brs" ] && brs=$(git unmerged); for br in $brs; do echo "# $br"; git branchmissingfrom $br; done; }; f'

# merge conflict
git config --global alias.mergecontext 'lg --merge --name-only'
git config --global alias.coco 'checkout --ours'
git config --global alias.coct 'checkout --theirs'
git config --global alias.cocd3 'checkout --conflict=diff3'

# reflog navigation
git config --global alias.undo '!f() { git reset --hard $(git rev-parse --abbrev-ref HEAD)@{${1:-1}}; }; f'

# grep
git config --global alias.g "grep --break --heading --line-number"

# clean
git config --global alias.pristine '!f() { git reset --hard ${1:-HEAD} && git clean -fdx; }; f'
git config --global alias.cleantmp 'clean -dX'


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
## all modification on a file code_file.rb
# log --follow code_file.rb
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
## branch diff
# git rev-list --left-right --count HEAD...origin/master
## bisect
# bisect start ; bisect bad HEAD ; bisect good HEAD~4 ; bisect good | bad ; bisect reset
# bisect start HEAD HEAD~4 ; git bisect run <make>
## diff
#  diff HEAD~2:Readme.md..HEAD:Readme.md

## branch track
# @{upstream} or @{u} shorthand references its upstream branch
# example: git merge @{u} instead of git merge origin/master

# dot Notation
## diff
## Two Dot Notation / Dot Dot Notation / A..B => range of commit between ref A and ref B
## log
## Two Dot Notation / Dot Dot Notation / A..B => commit reachable from ref B but not from ref A
## Three Dot Notation / Dot Dot Dot Notation / A...B => commit reachable for either of ref A or B but not from both


# browse
git config --global alias.browse '!f() { url=$(git config remote.origin.url | sed -e "s/^git@\([a-z.-]*\):\(.*\)$/https:\/\/\1\/\2/") ; xdg-open ${url}; }; f'

# merge limit
git config --global merge.renameLimit 999999

# version-bump
# git config --global alias.vb 'version-bump'

# gpg
#git config --global user.signingKey <gpg-keyid>
git config --global commit.gpgSign true
git config --global tag.gpgSign true

# visual
git config --global alias.visual '!gitk'

## mergetool
# list supported with: git mergetool --tool-help

######################################### doc

### tag
## delete remote tag
# git push origin :refs/tags/<tagname>
# git push origin --delete <tagname>


###############################################################################
# trash
###############################################################################
#git config --global alias.irebase '!f() { f=$(git symbolic-ref HEAD 2>/dev/null); f="origin/${f#refs/heads/}" ; git rebase --interactive "${1:-$f}"; }; f'
