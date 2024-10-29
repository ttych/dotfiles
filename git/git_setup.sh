#!/bin/sh
# -*- mode: sh -*-


### commit
#
# A good commit : A C I D
# - Atomic
#   self-contained:
#    Sementically related changes should not be split across commits.
#   coherent:
#    All change in a commit should be semantically related.
# - Consistent
#   no compilation errors:
#    A commit should not introduce compilation errors.
#   no broken tests:
#    A commit should not break any existing tests nor add failing ones.
#   -> should not introduce quality regressions
# - Incremental
#   ordered:
#    Commits should be ordered deliberately (not arbitrary).
#   explanatory:
#    The order should be a trail of the programmer's thought process.
# - Documented
#   a short summary:
#    A commit message should include a short short one-sentence summary.
#   detailed description:
#    A longer description can be added if more details are necessary.
#
# Commit Structure
#   commit
#    - parent (reference)
#    - the state of the files (snapshot)
#   snapshot
#    - directory (tree)
#    - file (blob)
#   reference
#    - tags (associated to a specific commit)
#    - branches (latest commit in a history line)
#    - HEAD (special reference to the snapshot of the current working directory)
#
### public vs private
#
# People can (and probably should) rebase their own work. That's a cleanup.
# But never other people's code. That's a "destroy history".
# -- Linux Torvalds
#
### rule of merging
#
#  public branch  to  public branch  => true merge
# private branch  to  public branch  => fast-forward merge
#
### commit reference
# by:
# - full sha1
# - beginning of sha1
# - branch
# - HEAD
# - HEAD^  (default parent of HEAD)
# - HEAD~2 (parent of parent of HEAD)
# - HEAD^2 (2nd parent of HEAD)
# - HEAD~2^2 (2nd parent of parent of parent of HEAD)
# - HEAD@{"1 month ago"} (value of HEAD 1 month ago)
#
# - @{upstream} or @{u} shorthand references its upstream branch
#   example: git merge @{u} instead of git merge origin/master
#
#
### dot..dot notation
#
## diff
# A..B  => range of commit from ref A to ref B
## log
# A..B  => commit reachable from ref B but not from ref A
# A...B => commit reachable from ref A or ref B but not from both
#
### git attributes / .gitattributes
#
# - http://bit.ly/git_language_parsers
#
###

which git >/dev/null 2>&1 || {
    echo "No git command found ! Aborting git configuration"
    exit 1
}


GIT_VERSION=`git --version`
GIT_VERSION=${GIT_VERSION##* }


######################################### config

# git config --local --list         # repo
# git config --global --list        # user
# git config --system --list        # system
#
# git config --get-all <setting>


### config :: user
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

### config :: edit
git config --global alias.configedit 'config --global -e'
git config --global alias.editconfig 'config --global -e'
git config --global alias.confe      'config --global -e'
git config --global alias.configlist 'config --list --show-origin'
git config --global alias.listconfig 'config --list --show-origin'
git config --global alias.confl      'config --list --show-origin'

### config :: includeIf
git config --global includeIf.gitdir:~/work/$USER/.path ~/work/$USER/.gitconfig
git config --global alias.workconfig '!f() { git config --global includeIf.gitdir:"${1%/}/".path "${1%/}/.gitconfig"; }; f'


######################################### gitignore
if [ -r "$HOME/.gitignore_global" ]; then
    git config --global core.excludesfile "$HOME/.gitignore_global"
fi


######################################### credentials
# git config --global credential.helper cache


######################################### core
### core :: color
git config --global color.ui auto
git config --global color.branch auto
git config --global color.diff auto
git config --global color.interactive auto
git config --global color.status auto
# blue foreground / black background / bold text
# git config --global color.diff.meta "blue black bold"
# text properties: bold, dim, ul (underline), blink, reverse

### core :: editor
for editor in e et emacs vim vi; do
    if command -v $editor >/dev/null 2>/dev/null; then
        git config --global core.editor $editor
        break
    fi
done

### core :: pager
git config --global core.pager 'less -RFX'
# git config --global core.pager ''

### core :: newline
## (linux) to convert CRLF to LF on commit but not the other way around
git config --global core.autocrlf input
## (windows) converts LF endings into CRLF when you check-out code
# git config --global core.autocrlf true
## (windows-only)
# git config --global core.autocrlf false

### core :: whitespace
## 6 whitespace issues
## 3 defaults: blank-at-eol,blank-at-eof,space-before-tab
## additional: cr-at-eol,tab-in-indent,indent-with-non-tab
## trailing-space = both blank-at-eol + blank-at-eof
git config --global core.whitespace 'trailing-space,space-before-tab'
git config --global alias.cleanwhitespace '!f() {git rebase HEAD~${1:-1} --whitespace=fix}; f'
git config --global alias.cleanblank '!f() {git rebase HEAD~${1:-1} --whitespace=fix}; f'


######################################### gc
### garbage collection cycle
## 90 days reachable / gc.reflogExpire
## 30 days unreachable / gc.reflogExpireUnreachable
git config --global alias.gcnow 'gc --prune=now --aggressive'
git config --global alias.gc-now 'gc --prune=now --aggressive'


######################################### help
### help :: autocorrect
# git config --global help.autocorrect 5  # 5/10 seconds


######################################### init
### init :: branch
git config --global init.defaultBranch master
# git config --global init.defaultBranch main
### init :: alias
# git config --global alias.i init


######################################### add
git config --global alias.a     "add"
git config --global alias.addi  "add --interactive"
git config --global alias.addp  "add --patch"


######################################### bisect
## bisect start ; bisect bad HEAD ; bisect good HEAD~4 ; bisect good || bisect bad ; bisect reset
## bisect start HEAD HEAD~4 ; git bisect run <make>


######################################### blame
# git blame -L14,25 <file>
git config --global alias.blamew  "blame -w"
## detect lines moved
# -C : in the same commit
# -C -C : in the same commit + commit that create file
# -C -C -C : in any commit
git config --global alias.blamec  "blame -w -C"


######################################### branch
# sort by: refname (def) | committerdate | creatordate | head | version:refname | ...
git config --global branch.sort -committerdate

git config --global alias.br branch
# git config --global alias.brv 'branch -v'
git config --global alias.brv 'branch -vv'
git config --global alias.bra 'branch --all'
# git config --global alias.brav 'branch --all -v'
git config --global alias.brav 'branch --all -vv'
git config --global alias.merged 'branch --merged'
git config --global alias.unmerged 'branch --no-merged'
git config --global alias.cleanmerged '!f() { for br in $(git branch --merged "$@" | egrep -v " master\$| main\$|^\*"); do echo git branch -d "$br"; done; }; f'
# git config --global alias.cleanmerged '!f() { git branch --merged ${1:-master} | grep -v " ${1:-master}$" | xargs git branch -d; }; f
git config --global alias.bru 'branch -u'

# git config --global alias.local-branches "!git branch -vv | cut -c 3- | awk '$3 !~/\\[/ { print $1 }'"
# git config --global alias.recent-branches '!git branch --sort=-committerdate | head'


######################################### checkout
git config --global alias.cob       'checkout -b'
git config --global alias.col       'checkout -'
git config --global alias.com       'checkout master'
git config --global alias.cop       'checkout --patch'
git config --global alias.cor       'checkout --track'
git config --global alias.orphan    'checkout --orphan'
git config --global alias.coours    'checkout --ours'
git config --global alias.cotheirs  'checkout --theirs'
git config --global alias.cocd3     'checkout --conflict=diff3'


######################################### cherry-pick
# excluded..included
git config --global alias.append '!f() { git cherry-pick $(git merge-base HEAD $1)..$1;}; f'


######################################### clean
git config --global alias.pristine '!f() { git reset --hard ${1:-HEAD} && git clean -fdx; }; f'
git config --global alias.cleantmp 'clean -dX'


######################################### clone
# git config --global alias.cl clone


######################################### column
# git config --global column.ui auto
# seq 1 24 | git column --mode=column --padding=5


######################################### commit
### commit :: template
if [ -r "$HOME/.gitmessage.txt" ]; then
    git config --global commit.template "$HOME/.gitmessage.txt"
fi
git config --global alias.ci commit
git config --global alias.cim "commit -m"
git config --global alias.cs "commit --signoff"
git config --global alias.cam "commit --all -m"
git config --global alias.amend 'commit --amend'
git config --global alias.extend 'commit --amend --no-edit'
git config --global alias.amendh 'commit --amend -C HEAD'
git config --global alias.fixup 'commit --fixup'
git config --global alias.squash 'commit --squash'


######################################### daemon
# git config --global alias.serve '!git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose'
# git config --global alias.servew '!git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --informative-errors --verbose'


######################################### diff
### diff :: algorithm : myers | minimal | patience | histogram
git config --global diff.algorithm patience

### diff strategy
# echo '*.png diff=exif' >> .gitattributes
# git config diff.exif.textconf exiftool

git config --global alias.di       'diff'
# git config --global alias.dc       'diff --check'
git config --global alias.ds       'diff --staged'
git config --global alias.diffs    'diff --staged'
git config --global alias.diffw    'diff --word-diff'
git config --global alias.diffword 'diff --word-diff'
git config --global alias.difft    'diff --word-diff --unified=10'
git config --global alias.difftext 'diff --word-diff --unified=10'

# git diff HEAD~2:Readme.md..HEAD:Readme.md

git config --global alias.lsstg     'diff --name-only --staged'

git config --global alias.conflicts 'diff --name-only --diff-filter=U'
git config --global alias.dlc 'diff --cached HEAD^'
git config --global alias.dic '!f() { git diff "${1:-HEAD}"^.."${1:-HEAD}"; }; f'



######################################### exec
git config --global alias.exec '!exec '


######################################### fetch
# git config --global alias.fe fetch


######################################### fsck
# without reflogs
git config --global alias.dangling 'fsck --dangling --no-reflogs --no-progress'
git config --global alias.unreachable 'fsck --unreachable --no-reflogs --no-progress'
# purge-reflog
git config --global alias.purge-reflog 'reflog expire --expire=now --all'


######################################### filter-branch
git config --global alias.purge-file '!f() { git filter-branch --force --index-filter "git rm --cached --ignore-unmatch $1" --prune-empty --tag-name-filter cat -- --all ; }; f'


######################################### gpg
## ssh
if [ -r ~/.ssh/git_key.pub ]; then
    git config --global gpg.format ssh
    git config --global user.signingkey ~/.ssh/git_key.pub
fi

## gpg
#git config --global user.signingKey <gpg-keyid>

## sign
git config --global commit.gpgSign true
# git config --global tag.gpgSign true
# git config --global push.gpgSign true


######################################### grep
git config --global alias.gr "grep --break --heading --line-number"
# git config --global alias.search '!f() { git rev-list --all | xargs git grep -F "$1"; }; f'


######################################### log
git config --global log.decorate auto

### pretty formater
# https://git-scm.com/docs/pretty-formats

git config --global alias.lg   'log'
git config --global alias.l1   'log --oneline --decorate'
git config --global alias.l1s  'log --oneline --decorate --numstat'
git config --global alias.l1g  'log --oneline --decorate --graph --all'
git config --global alias.l10  'log --oneline --decorate -10'
git config --global alias.l10s 'log --oneline --decorate -10'
git config --global alias.l10g 'log --oneline --decorate -10 --graph --all'
git config --global alias.l11  'log --oneline --decorate -1'
git config --global alias.ll   'log --decorate --all'
git config --global alias.lls  'log --decorate --all --numstat'
git config --global alias.llg  'log --decorate --all --graph'
git config --global alias.logf 'log --pretty=fuller'
# git config --global alias.lo   'log --oneline --decorate'
# git config --global alias.lol  'log --oneline --decorate --graph'
# git config --global alias.lola 'log --oneline --decorate --graph --all'
git config --global alias.l2 "log --pretty='%C(yellow)%h%Creset %s %C(green)(%cr)%Creset %C(cyan)[%an]%Creset%C(red)%d%Creset'"
# git config --global alias.l2g "log --pretty='%C(red)%h%Creset | %C(yellow)%d%Creset %s %C(green)(%cr)%Creset %C(cyan)[%an]%Creset' --graph --all"
# git config --global alias.l2 "log --pretty='%C(yellow)%h %Cred%cr %Cblue(%an)%C(white)%d%Creset %s'"

# git config --global alias.l3   'log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
# git config --global alias.l3s  'log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'
# git config --global alias.l3ds 'log --pretty=format:"%C(yellow)%h\\ %ad%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --date=short
# git config --global alias.l31  '!git l3s -1'

git config --global alias.loc  '!f() { git l1s "${1:-HEAD}"^.."${1:-HEAD}"; }; f'

### search by word in commit message
# git log --grep apples
### search by word in commit content
# git log -G<word> --patch

### range of commit between 2 refs B and A (decendent of B / ancestor of A)
## git log --ancestry-path B..A

### branch-diff
git config --global alias.branchdiff '!f() { [ -z "$2" ] && set -- "" "$1"; git l1 --cherry-mark --left-right --no-merges "$1"..."$2"; }; f'
git config --global alias.brdiff '!git branchdiff'
# excluding equivalent
git config --global alias.branchdif '!f() { [ -z "$2" ] && set -- "" "$1"; git l1 --cherry-pick --left-right --no-merges "$1"..."$2"; }; f'
git config --global alias.brdif '!git branchdif'
#
git config --global alias.branchdiffl '!f() { [ -z "$2" ] && set -- HEAD "$1"; git l1 --cherry-mark --left-only --no-merges "$1"..."$2"; }; f'
git config --global alias.brdiffl '!git branchdiffl'
git config --global alias.branchdiffr '!f() { [ -z "$2" ] && set -- HEAD "$1"; git l1 --cherry-mark --right-only --no-merges "$1"..."$2"; }; f'
# same as
# git config --global alias.branchdiffr '!f() { [ -z "$2" ] && set -- HEAD "$1"l git l1 --cherry "$1"..."$2"; }; f'
git config --global alias.brdiffr '!git branchdiffr'

### log merge commits only from A into B
# git log --merges A..B
### log merge commits descendant of A and ancestor of B
# git log --merges --ancestry-path A..B

### unmerged
# git config --global alias.unmergedc '!f() { brs="$1" ; [ -z "$brs" ] && brs=$(git unmerged); for br in $brs; do echo "# $br"; git lg HEAD..$br; done; }; f'
# git config --global alias.unmergedc '!f() { brs="$1" ; [ -z "$brs" ] && brs=$(git unmerged); for br in $brs; do echo "# $br"; git branchmissingfrom $br; done; }; f'


### file tracking
git config --global alias.log4file '!git -C ${GIT_PREFIX:-.} l1 --follow'
git config --global alias.log4filep '!git -C ${GIT_PREFIX:-.} l1 --follow --patch'
# log -Sstring
# log -S".*pattern.*" --pickaxe-rege
# git config --global alias.log4str '!f() { git -C ${GIT_PREFIX:-.} l1 -S"$1"; }; f'
# log -G".*pattern.*"
git config --global alias.log4str '!f() { git -C ${GIT_PREFIX:-.} l1 -G"$1"; }; f'
git config --global alias.log4rgx '!f() { git -C ${GIT_PREFIX:-.} l1 -G"$1"; }; f'
git config --global alias.pickaxe '!f() { git -C ${GIT_PREFIX:-.} l1 -G"$1" "$@"; }; f'
git config --global alias.pickaxep '!f() { git -C ${GIT_PREFIX:-.} l1 -G"$1" --patch "$@"; }; f'
# git log -L14,25:<file>
# git log -L:<function>:<file>
# alias.log4func <func> <file>
git config --global alias.log4func '!f() { git -C ${GIT_PREFIX:-.} l1 -L:"${1}${2:+:$2}"; }; f'
# rely on .gitattributes

### merge conflict
git config --global alias.logmerge 'log --merge --name-only'

### reflogs
git config --global alias.logr   'log --walk-reflogs'
git config --global alias.logref 'log --walk-reflogs'
# git log --grep=<word in commit> --walk-reflogs
git config --global alias.l1ref  'l1 --walk-reflogs'


######################################### ls-files
git config --global alias.ls            'ls-files'
git config --global alias.lsindex       'ls-files --stage'
git config --global alias.lsmod         'ls-files --modified'
git config --global alias.lsunt         'ls-files --others --exclude-standard'
git config --global alias.lsign         'ls-files --others --ignored --exclude-standard'

# git config --global alias.ls-files-root '!git ls-files'
git config --global alias.ls-files-root 'ls-files --full-name'
git config --global alias.lsr           'ls-files-root'
git config --global alias.lsrmod        'ls-files-root --modified'
git config --global alias.lsrunt        'ls-files-root --others --exclude-standard'
git config --global alias.lsrign        'ls-files-root --others --ignored --exclude-standard'
# git config --global alias.lsf           '!f() { lsf_path=:/ ; for arg; do case $arg in -*) ;; *) lsf_path= ;; esac ; done ; git ls-files "$@" $lsf_path ; }; f'

git config --global alias.lst           'ls-tree -r'


######################################### maintenance
## global
# git config --global maintenance.auto true
# git config --global maintenance.commit-graph.auto true
# git config --global maintenance.schedule daily
# git config --global maintenance.repack.incremental true
# git config --global maintenance.repack.full true
# git config --global maintenance.repack.full.schedule weekly
## by repo
# git maintenance [start | status | stop]
## Enable daily incremental repacking
# git config maintenance.repack.incremental true
# git config maintenance.repack.schedule daily
## Enable weekly full repacking
# git config maintenance.repack.full true
# git config maintenance.repack.full.schedule weekly


######################################### merge
git config --global merge.conflictStyle diff3
# git config --global merge.renameLimit 999999

# git config --global alias.mergeto '!f() { git checkout $1 && git merge @{-1} && git checkout @{-1} ;}; f'
git config --global alias.mergeto '!f() { git checkout $1 && git merge @{-1}; }; f'
git config --global alias.truemerge "merge --no-ff"
git config --global alias.ffmerge "merge --ff-only"


######################################### merge-base
# closest common ancestor
## merge-base HEAD MERGE_HEAD


######################################### plumb
git config --global alias.gos     'cat-file -s'
git config --global alias.gosize  'cat-file -s'
git config --global alias.goc     'cat-file -p'
git config --global alias.gocat   'cat-file -p'
git config --global alias.plumb   'cat-file -p'
git config --global alias.got     'cat-file -t'
git config --global alias.gotype  'cat-file -t'


######################################### pull
git config --global alias.ffpull "pull --no-rebase --ff-only"
git config --global alias.repull "pull --rebase"
git config --global alias.up '!git pull --rebase $@ && git submodule update --init --recursive'

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


######################################### push
# push : nothing | matching | simple | current
git config --global push.default current

git config --global alias.push2all      '!f() { for remote in `git remote`; do git push $remote "$@"; done }; f'
git config --global alias.pushf         'push --force-with-lease'
git config --global alias.rm-remote-ref '!f() { [ $# -ge 2 ] && rem="$1" && shift ; git push "${rem:-origin}" --delete "$@"; }; f'
# alternativ is: git push <origin> :refs/tags/<v1.4-lw>


######################################### rebase
### automatically update branch references
# git config --global rebase.updateRefs true
### Rebase + autosquash
## causes git rebase -i to parse magic comments created
## by git commit --squash=some-hash and git commit --fixup=some-hash
## and reorder the commit list before presenting it for further editing
git config --global rebase.autosquash true

git config --global alias.rb   'rebase'
git config --global alias.rbi  'rebase -i'
git config --global alias.tidy 'rebase -i @{upstream}'
git config --global alias.rbif 'rebase -i --fork-point'
# equivalent to
# git rebase -i $(git merge-base --fork-point <branch>)
git config --global alias.rbp '!f() { git log --reverse --pretty=format:"%h %s" ${1}..HEAD; }; f'
git config --global alias.rbprev '!f() { git log --reverse --pretty=format:"%h %s" ${1}..HEAD; }; f'
git config --global alias.rbiroot 'rebase -i --root'
# git config --global alias.rbiroot 'rebase -i --root --autosquash'


######################################### reflog
# <reference>@{index}
git config --global alias.undo '!f() { git reset --hard $(git rev-parse --abbrev-ref HEAD)@{${1:-1}}; }; f'


######################################### rerere
### REuse REcorded REsolution
git config --global rerere.enabled true


######################################### reset
## --hard / update : HEAD + index + workspace (destructive)
## --mixed (default) / update : HEAD + index
## --soft / update : HEAD
git config --global alias.unstage 'reset --mixed HEAD --'
git config --global alias.resetto '!f() { branch=$(git rev-parse --abbrev-ref HEAD); git reset --hard "${1:-$branch}"; }; f'
git config --global alias.resetfile '!f() { git reset @~ "$@" && git commit --amend --no-edit }; f'


######################################### remote
### remote fetch
# git config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'
# git config --add remote.origin.fetch '+refs/pull/*:refs/remotes/origin/pull/*'
git config --global alias.remfpr '!f() { config --add remote.${1:-origin}.fetch "+refs/pull/*:refs/remotes/origin/pull/*"; }; f'

git config --global alias.rem      'remote -v'
git config --global alias.remshow  'remote -v show'
git config --global alias.lsrem    'ls-remote'


######################################### restore
# git config --global alias.unstage 'restore --staged --'
# git config --global alias.unmodify 'restore --'
# = git restore <file.txt>
git config --global alias.rt   'restore'
# = git checkout -p
git config --global alias.rtp  'restore -p'
# = git checkout HEAD@{10.minutes.ago} -- <file.txt>
# git restore --source HEAD@{10.minutes.ago} <file.txt>


######################################### rev-list
# git rev-list --left-right --count HEAD...origin/master


######################################### rm
git config --global alias.untrack 'rm --cache --'


######################################### root
# git config --global alias.root 'rev-parse --show-toplevel'
git config --global --add alias.rootdir '!pwd'


######################################### scalar
# clone for huge repository
# scalar clone git@github.com:.../....git
# - prefetching
# - commit-graph
# - filesystem monitor
# - partial cloning
# - sparse checkout


######################################### shortlog
# contributors
git config --global alias.contribs 'shortlog -s -n'
git config --global alias.contributors 'shortlog -s -n'
# git config --global alias.authors "!git log --format='%aN <%aE>' | grep -v 'users.noreply.github.com' | sort -u --ignore-case"


######################################### show
git config --global alias.so "show --pretty='parent %C(yellow)%p%Creset commit %C(yellow)%h%Creset%Cred%d%Creset%n%n%w(72,2,2)%s%n%n%w(72,0,0)%C(cyan)%an%Creset %Cgreen%ar%Creset'"


######################################### show-branch
git config --global alias.sb 'show-branch'
git config --global alias.sbt '!f() { bt="$1"; [ $# -gt 0 ] && shift ; [ -z "$bt" ] && bt=$(git rev-parse --abbrev-ref HEAD); git show-branch --topic $bt "$@"; }; f'
## git show-branch <br1> <br2>
## git show-branch --topic master <br1> <br2>


######################################### show-ref
git config --global alias.ref    'show-ref'
git config --global alias.refs   'show-ref'
# cat .git/refs/heads/<head>
git config --global alias.heads  'show-ref --heads'
git config --global alias.rheads 'show-ref --heads'
# cat .git/refs/tags/<tag>
git config --global alias.rtags  'show-ref --tags'


######################################### submodule
git config --global submodule.recurse true
git config --global diff.submodule log
git config --global status.submodulesummary 1
git config --global push.recurseSubmodules no

git config --global alias.sinit 'submodule update --init --recursive'
git config --global alias.sco 'checkout --recurse-submodules'
git config --global alias.sdiff '!'"git diff && git submodule foreach 'git diff'"
git config --global alias.spush 'push --recurse-submodules=check'
git config --global alias.supdate 'submodule update --remote --merge'


######################################### stash = index + workspace (default)
git config --global alias.stash1   'stash save --keep-index --include-untracked "> Working"'
git config --global alias.stashw   'stash save --keep-index --include-untracked "> Working"'
git config --global alias.stash2   'stash save --include-untracked "> Index"'
git config --global alias.stashi   'stash save --include-untracked "> Index"'
git config --global alias.stashs   'stash save --include-untracked "> Index"'
git config --global alias.stash3   'stash save --include-untracked --all "> All"'
git config --global alias.stashall 'stash save --include-untracked --all "> All"'
git config --global alias.staash    'stash save --include-untracked --all "> All"'
git config --global alias.stash2br 'stash branch'


######################################### status
git config --global alias.st status
git config --global alias.s "status --short --branch"
# git config --global advice.statusuoption false


######################################### switch
# = git checkout
git config --global alias.sw  'switch'
# = git checkout -b
git config --global alias.swc 'switch -c'
# = git checkout -
git config --global alias.swl 'switch -'


######################################### tag
git config --global alias.ctag 'describe --exact-match --tags'
## delete remote tag
# git push origin :refs/tags/<tagname>
# git push origin --delete <tagname>


######################################### worktree
# working on more than one branch at a time
# provide a new working directory for each branch


######################################### large repository
## prefect
## commit-graph
# git config --global fetch.writeCommitGraph true
## filesystem monitor
# git config core.untrackedcache true
# git config core.fsmonitor true
## ref
# https://github.blog/2021-04-29-scaling-monorepo-maintenance/


######################################### hook : client-side (11)
## Commit Stuff
# pre-commit
# prepare-commit-msg
# commit-msg
# post-commit
## Rewriting Stuff
# pre-rebase
# post-rewrite
## Merging Stuff
# post-merge
# pre-merge-commit
## Switching/Pushing Stuff
# post-checkout
# reference-transaction
# pre-push


######################################### extra
# browse
git config --global alias.browse '!f() { url=$(git config remote.origin.url | sed -e "s/^git@\([a-z.-]*\):\(.*\)$/https:\/\/\1\/\2/") ; xdg-open ${url}; }; f'
# ui
git config --global alias.ui '!gitk'
# git-better-branch
git config --global alias.bb '!git-better-branch'
