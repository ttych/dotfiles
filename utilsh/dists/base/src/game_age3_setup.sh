#!/bin/sh

WINE_NAME=age3
GAME_DIR="${GAME_DIR:-$HOME/mnt/age3}"

do_wwine()
{
    rm -Rf "$HOME/.wwine/$WINE_NAME" &&
        wwine "$WINE_NAME" setup win32
}

do_wwine_tricks()
{
    wwine "$WINE_NAME" tricks -q amstream d3dx9 d3dx9_43 d3dxof devenum dinput8 dirac directmusic directplay dmsynth dotnet20 dsound dxdiag dxdiagn_feb2010 ffdshow l3codecx msxml4 quartz riched20 riched30 vb5run vcrun2003 vcrun2005 vcrun2008 vcrun2010 vcrun6 xvid
    # icodecs
}

do_install()
{
    wwine age3 exe "$GAME_DIR/install.exe" &&
        cp "$HOME/.wwine/$WINE_NAME/drive_c/Program Files/Microsoft Games/Age of Empires III/age3.exe" "$HOME/.wwine/$WINE_NAME/drive_c/Program Files/Microsoft Games/Age of Empires III/age3.exe.bkp" &&
        cp "$GAME_DIR/fix/age3.exe" "$HOME/.wwine/$WINE_NAME/drive_c/Program Files/Microsoft Games/Age of Empires III/age3.exe" # &&
        # wwine age3 exe "$GAME_DIR/patches/aoe3-114-french.exe"
}

do_all()
{
    do_wwine &&
        do_wwine_tricks &&
        do_install
}

case ${1:-install} in
    all)
        do_all
        ;;
    wwine)
        do_wwine
        ;;
    tricks)
        do_wwine_tricks
        ;;
    install)
        do_install
        ;;
esac
