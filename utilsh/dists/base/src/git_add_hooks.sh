#!/bin/sh

# validate commit message
## < 51 chars
wget -q http://git.io/validate-commit-msg -O .git/hooks/commit-msg
chmod +x .git/hooks/commit-msg




## git : post-receive
## do : checkout
##!/bin/sh
#GIT_WORK_TREE=/home/user/app git checkout -f
#cd /home/user/app && bundle
#...
