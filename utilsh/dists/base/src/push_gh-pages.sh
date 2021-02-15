#!/bin/sh

# usage:
# push_gh-pages (from:)<dir> (to:)<branch>

PUSH_DIR="${1:-_site}"
PUSH_BRANCH="${2:-gh-pages}"

PUSH_STATUS=0
PUSH_INIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)

git branch -a | grep origin/${BRANCH} >/dev/null &&
    git branch -f -d "${PUSH_BRANCH}" || exit 1

git --git-dir=.git --work-tree="${PUSH_DIR}" checkout --orphan "${PUSH_BRANCH}" &&
    git --git-dir=.git --work-tree="${PUSH_DIR}" add --all &&
    git --git-dir=.git --work-tree="${PUSH_DIR}" commit --allow-empty -m "autogen: update site" &&
    git --git-dir=.git --work-tree="${PUSH_DIR}" push -f origin "${PUSH_BRANCH}" || {
        echo "failure ..."
        PUSH_STATUS=1
    }

git --git-dir=.git checkout -f "${PUSH_INIT_BRANCH}"

exit "$PUSH_STATUS"
