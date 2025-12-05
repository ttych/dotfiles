#!/bin/sh

DISTRIBUTIONS='
  https://github.com/ttych/publish.git
  https://github.com/ttych/pyv.git
  https://github.com/ttych/wwine.git
  https://github.com/ttych/git-hooks.git
'

OLD_DISTRIBUTIONS='
  https://github.com/ttych/installer.git
  https://github.com/ttych/wec.git
  https://github.com/ttych/wtmux.git
'

AUTOLOADS='
  std.shl
  python_env.shl
  pyv.sh
  git.lib.sh
  editors.lib.sh
'

OLD_AUTOLOADS='
  ruby_env.shl
  texlive.lib.sh
'


if [ -z "$UTILSH_DIR" ]; then
    UTILSH_DIR=`cd ${0%/*} ; echo $PWD`
fi

install_distributions()
{
    for distribution in $DISTRIBUTIONS; do
        "$UTILSH_DIR"/bin/utilsh distribution add "$distribution"
    done
}

install_autoloads()
{
    for autoload in $AUTOLOADS; do
        "$UTILSH_DIR"/bin/utilsh autoload "$autoload"
    done
}

install_distributions
install_autoloads
