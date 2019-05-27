#!/bin/sh
# -*- mode: sh -*-


#%%load%% os.shl
#%%load%% dist_build.shl


#


#> ruby - ruby interpreter
BUILD_TOOLS_RUBY_PREREQ_REDHAT='readline-devel openssl-devel zlib-devel sqlite-devel'
BUILD_TOOLS_RUBY_PREREQ_UBUNTU='libreadline-dev libssl-dev libsqlite3-dev'
BUILD_TOOLS_RUBY_URL_PATTERN='http://cache.ruby-lang.org/pub/ruby/ruby-${version}.tar.gz'
BUILD_TOOLS_RUBY_DEFAULT_VERSION='2.4.3'

#> python - python interpreter
BUILD_TOOLS_PYTHON_PREREQ_REDHAT='readline-devel openssl-devel libffi-devel tk-devel sqlite-devel bzip2-devel'
BUILD_TOOLS_PYTHON_PREREQ_UBUNTU='libreadline-dev libssl-dev libffi-dev tk-dev libsqlite3-dev libbz2-dev'
BUILD_TOOLS_PYTHON_URL_PATTERN='https://www.python.org/ftp/python/${version}/Python-${version}.tgz'
BUILD_TOOLS_PYTHON_DEFAULT_VERSION='3.6.6'

#> ag - the silver searcher
BUILD_TOOLS_AG_PREREQ_REDHAT='xz-devel'
BUILD_TOOLS_AG_PREREQ_UBUNTU='liblzma-dev libpcre++-dev'
BUILD_TOOLS_AG_URL_PATTERN='http://geoff.greer.fm/ag/releases/the_silver_searcher-${version}.tar.gz'
BUILD_TOOLS_AG_DEFAULT_VERSION='0.32.0'

#> sublime - sublime text 3
BUILD_TOOLS_SUBLIME_URL_PATTERN='https://download.sublimetext.com/sublime_text_${version}.tar.bz2'
BUILD_TOOLS_SUBLIME_DEFAULT_VERSION='3_build_3176_x64'

#> rubymine - RubyMine
BUILD_TOOLS_RUBYMINE_URL_PATTERN='https://download.jetbrains.com/ruby/RubyMine-${version}.tar.gz'
BUILD_TOOLS_RUBYMINE_DEFAULT_VERSION='2018.3.5'

#> pycharm - PyCharm
BUILD_TOOLS_PYCHARM_URL_PATTERN='https://download.jetbrains.com/python/pycharm-${version}.tar.gz'
BUILD_TOOLS_PYCHARM_DEFAULT_VERSION='community-2018.1.3'

#> heroku - heroku toolbelt
BUILD_TOOLS_HEROKU_URL_PATTERN='https://s3.amazonaws.com/assets.heroku.com/heroku-client/heroku-client.tgz'
BUILD_TOOLS_HEROKU_DEFAULT_VERSION='none'

#> ctwm - ctwm window manager
BUILD_TOOLS_CTWM_PREREQ_UBUNTU='cmake libxmu-dev libx11-dev libxpm-dev libjpeg-dev'
BUILD_TOOLS_CTWM_PREREQ_REDHAT='cmake libXmu-devel libX11-devel libXpm-devel libjpeg-turbo-devel'
BUILD_TOOLS_CTWM_URL_PATTERN='https://www.ctwm.org/dist/ctwm-${version}.tar.gz'
BUILD_TOOLS_CTWM_DEFAULT_VERSION='4.0.1'

#> xbindkeys - shortcut utility
BUILD_TOOLS_XBINDKEYS_CONFIG='--disable-guile'
#BUILD_TOOLS_XBINDKEYS_PREREQ_UBUNTU='guile-2.0 guile-2.0-dev'
BUILD_TOOLS_XBINDKEYS_URL_PATTERN='http://www.nongnu.org/xbindkeys/xbindkeys-${version}.tar.gz'
BUILD_TOOLS_XBINDKEYS_DEFAULT_VERSION='1.8.6'

#> emacs - emacs editor
#BUILD_TOOLS_EMACS_CONFIG='--with-x-toolkit=gtk2 --with-gif=no --with-x=yes'
BUILD_TOOLS_EMACS_CONFIG='--with-x-toolkit=gtk3 --with-gif=no --with-x=yes --with-gnutls=yes'
BUILD_TOOLS_EMACS_PREREQ_REDHAT='gtk2-devel libtiff-devel gnutls-devel'
BUILD_TOOLS_EMACS_PREREQ_UBUNTU='libtiff5-dev libgif-dev libgnutls28-dev libgtk-3-dev libncurses-dev libxpm-dev'
BUILD_TOOLS_EMACS_URL_PATTERN='http://ftpmirror.gnu.org/emacs/emacs-${version}.tar.gz'
BUILD_TOOLS_EMACS_DEFAULT_VERSION='26.1'

#> emacs-nox - emacs editor without X
BUILD_TOOLS_EMACS_NOX_CONFIG='--with-x=no --with-gif=no --with-gnutls=yes'
BUILD_TOOLS_EMACS_NOX_PREREQ_REDHAT='libtiff-devel gnutls-devel'
BUILD_TOOLS_EMACS_NOX_PREREQ_UBUNTU='libtiff5-dev libgif-dev libgnutls28-dev libncurses-dev'
BUILD_TOOLS_EMACS_NOX_URL_PATTERN='http://ftpmirror.gnu.org/emacs/emacs-${version}.tar.gz'
BUILD_TOOLS_EMACS_NOX_DEFAULT_VERSION='26.1'

#> vscode - Visual Studio Code
BUILD_TOOLS_VSCODE_PREREQ_REDHAT='libXScrnSaver'
BUILD_TOOLS_VSCODE_PREREQ_UBUNTU='libgconf-2-4'
BUILD_TOOLS_VSCODE_URL_PATTERN='https://az764295.vo.msecnd.net/stable/f46c4c469d6e6d8c46f268d1553c5dc4b475840f/code-stable-${version}.tar.gz'
BUILD_TOOLS_VSCODE_DEFAULT_VERSION='1536736541'

#> guile - guile
BUILD_TOOLS_GUILE_PREREQ_REDHAT='libtool-ltdl-devel gmp-devel'
BUILD_TOOLS_GUILE_URL_PATTERN='https://ftp.gnu.org/gnu/guile/guile-${version}.tar.xz'
BUILD_TOOLS_GUILE_DEFAULT_VERSION='2.2.3'

#> geckodriver - geckodriver
BUILD_TOOLS_GECKODRIVER_URL_PATTERN='https://github.com/mozilla/geckodriver/releases/download/${version}/geckodriver-${version}-linux64.tar.gz'
BUILD_TOOLS_GECKODRIVER_DEFAULT_VERSION='v0.20.1'

#> keepassx - KeePassX
BUILD_TOOLS_KEEPASSX_PREREQ_REDHAT='qt-devel libgcrypt-devel zlib-devel gcc-c++'
BUILD_TOOLS_KEEPASSX_PREREQ_UBUNTU='libgcrypt20-dev'
BUILD_TOOLS_KEEPASSX_URL_PATTERN='https://www.keepassx.org/releases/${version}/keepassx-${version}.tar.gz'
BUILD_TOOLS_KEEPASSX_DEFAULT_VERSION='2.0.3'

#> keepassxc - KeePassXC
BUILD_TOOLS_KEEPASSXC_URL_PATTERN='https://github.com/keepassxreboot/keepassxc/releases/download/${version}/KeePassXC-${version}-x86_64.AppImage'
BUILD_TOOLS_KEEPASSXC_DEFAULT_VERSION='2.4.1'

#> cmake - CMake
BUILD_TOOLS_CMAKE_URL_PATTERN='https://github.com/Kitware/CMake/releases/download/v${version}/cmake-${version}.tar.gz'
BUILD_TOOLS_CMAKE_DEFAULT_VERSION='3.14.2'

#> nodejs - nodejs
BUILD_TOOLS_NODEJS_PREREQ_REDHAT=
BUILD_TOOLS_NODEJS_PREREQ_UBUNTU=
BUILD_TOOLS_NODEJS_URL_PATTERN='https://nodejs.org/dist/${version}/node-${version}-linux-x64.tar.xz'
BUILD_TOOLS_NODEJS_DEFAULT_VERSION='v10.15.0'

#> jdk11 - jdk11
BUILD_TOOLS_JDK11_PREREQ_REDHAT=
BUILD_TOOLS_JDK11_PREREQ_UBUNTU=
BUILD_TOOLS_JDK11_URL_PATTERN='http://download.oracle.com/otn-pub/java/jdk/11.0.1+13/90cf5d8f270a4347a95050320eef3fb7/jdk-${version}_bin.tar.gz'
BUILD_TOOLS_JDK11_DEFAULT_VERSION='11.0.1_linux-x64'
BUILD_TOOLS_JDK11_URL_HEADER='Cookie: oraclelicense=accept-securebackup-cookie'

#> jdk8 - jdk8
BUILD_TOOLS_JDK8_PREREQ_REDHAT=
BUILD_TOOLS_JDK8_PREREQ_UBUNTU=
BUILD_TOOLS_JDK8_URL_PATTERN='http://download.oracle.com/otn-pub/java/jdk/8u191-b12/2787e4a523244c269598db4e85c51e0c/jdk-${version}.tar.gz'
BUILD_TOOLS_JDK8_DEFAULT_VERSION='8u191-linux-x64'
BUILD_TOOLS_JDK8_URL_HEADER='Cookie: oraclelicense=accept-securebackup-cookie'

#> android_studio - Android Studio
BUILD_TOOLS_ANDROID_STUDIO_PREREQ_REDHAT=
BUILD_TOOLS_ANDROID_STUDIO_PREREQ_UBUNTU=
BUILD_TOOLS_ANDROID_STUDIO_URL_PATTERN='https://dl.google.com/dl/android/studio/ide-zips/3.2.1.0/android-studio-ide-${version}-linux.zip'
BUILD_TOOLS_ANDROID_STUDIO_DEFAULT_VERSION='181.5056338'
#BUILD_TOOLS_ANDROID_STUDIO_URL_HEADER='Cookie: oraclelicense=accept-securebackup-cookie'

#> groovy - groovy interpreter
BUILD_TOOLS_GROOVY_PREREQ_REDHAT=
BUILD_TOOLS_GROOVY_PREREQ_UBUNTU=
BUILD_TOOLS_GROOVY_URL_PATTERN='https://bintray.com/artifact/download/groovy/maven/apache-groovy-binary-${version}.zip'
BUILD_TOOLS_GROOVY_DEFAULT_VERSION='2.5.4'

#> tmux - tmux
BUILD_TOOLS_TMUX_PREREQ_REDHAT='libevent-devel'
BUILD_TOOLS_TMUX_PREREQ_UBUNTU='libevent-dev'
BUILD_TOOLS_TMUX_URL_PATTERN='https://github.com/tmux/tmux/releases/download/${version}/tmux-${version}.tar.gz'
BUILD_TOOLS_TMUX_DEFAULT_VERSION='2.8'

#> jq - jq
BUILD_TOOLS_JQ_CONFIG='--without-oniguruma'
BUILD_TOOLS_JQ_PREREQ_REDHAT=
BUILD_TOOLS_JQ_PREREQ_UBUNTU=
BUILD_TOOLS_JQ_URL_PATTERN='https://github.com/stedolan/jq/releases/download/jq-${version}/jq-${version}.tar.gz'
BUILD_TOOLS_JQ_DEFAULT_VERSION='1.6'


build_tools_env()
{
    build_tools_env__name="`echo ${1} | tr '[a-z]-' '[A-Z]_'`"

    eval build_tools_env__url_pattern="\"\$BUILD_TOOLS_${build_tools_env__name}_URL_PATTERN\""
    [ -z "$build_tools_env__url_pattern" ] && return 1
    eval build_tools_env__d_version="\"\$BUILD_TOOLS_${build_tools_env__name}_DEFAULT_VERSION\""
    build_tools_env__version="${2:-$build_tools_env__d_version}"
    [ -z "$build_tools_env__version" ] && return 1
    eval build_tools_env__url_header="\"\$BUILD_TOOLS_${build_tools_env__name}_URL_HEADER\""

    name="$build_tools_env__name"
    version="$build_tools_env__version"
    eval build_tools_env__url="\"$build_tools_env__url_pattern\""

    build_tools_env__os="`echo ${OS_NAME} | tr '[a-z]' '[A-Z]'`"
    eval build_tools_env__prereq="\"\$BUILD_TOOLS_${build_tools_env__name}_PREREQ_${build_tools_env__os}\""

    eval build_tools_env__config="\"\$BUILD_TOOLS_${build_tools_env__name}_CONFIG\""
}

build_tools()
{
    dist_identify_location "$1" && shift
    build_tools__loc="$dist_identify_location"

    [ -z "$1" ] && return 1
    build_tools_env "$1" "$2" || return 1

    os_install $build_tools_env__prereq || return 1
    dist_build $build_tools__loc "$build_tools_env__url" "$build_tools_env__config" "$build_tools_env__url_header"
}

download_tools_to_build()
{
    [ -z "$1" ] && return 1
    build_tools_env "$1" "$2" || return 1

    dist_download "$build_tools_env__url" "$build_tools_env__url_header"
}

#

if [ "$1" = '-h' ] || [ $# -eq 0 ]; then
    printf '%s\n' "Usage is : $0 [-h] [-dl] [try_alt|alt|loc=|try_loc=] tool1[:version] [tool2 ...]"
    printf '%s\n' 'Available tools are :'
    grep '^#> ' "$0"
    exit 0
fi

download_only=FALSE
if [ "$1" = '-dl' ]; then
    download_only=TRUE
    shift
fi

dist_identify_location "$1" && shift
loc="$dist_identify_location"

for to_build; do
    to_build__version="${to_build##*:}"
    [ "$to_build__version" = "$to_build" ] && to_build__version=
    to_build__name="${to_build%:*}"

    if $download_only; then
        download_tools_to_build "${to_build__name}" "${to_build__version}"
    else
        build_tools $loc "${to_build__name}" "${to_build__version}"
    fi
done
