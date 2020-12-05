#!/bin/sh


terminal_esc()
{
    printf "%b\n" \
           "ESC=\"\e\"" \
           "export ESC"
}

eval "$(terminal_esc)"


## attributes: printf “$TERMINAL_SET_ATTR” $TERMINAL_BOLD
TERMINAL_BOLD=1
TERMINAL_DIM=2        ## may not be supported
TERMINAL_UNDERLINE=4
TERMINAL_BLINK=5      ## may not be supported
TERMINAL_REVERSE=7
TERMINAL_HIDDEN=8     ## may not be supported

## screen codes
TERMINAL_CSI=${ESC}[                           ## Control Sequence Introducer
TERMINAL_NA=${TERMINAL_CSI}0m                  ## Reset attributes to the default
TERMINAL_CLS=${TERMINAL_CSI}H${TERMINAL_CSI}2J ## Clear the screen
TERMINAL_CLE=${TERMINAL_ESC}[K                 ## Clear to end of line

## cursor movement
TERMINAL_CU_ROW_COL="${TERMINAL_CSI}%d;%dH"    ## position cursor by row and column
TERMINAL_CU_UP=${TERMINAL_CSI}%dA              ## mv cursor up N lines
TERMINAL_CU_DOWN=${TERMINAL_CSI}%dB            ## mv cursor down N lines
TERMINAL_CU_D1=${TERMINAL_CSI}1B               ## mv cursor down 1 line
TERMINAL_CU_RIGHT=${TERMINAL_CSI}%dC           ## mv cursor right N columns
TERMINAL_CU_LEFT=${TERMINAL_CSI}%dD            ## mv cursor left N columns
TERMINAL_CU_SAVE=${ESC}7                       ## save cursor position
TERMINAL_CU_RESTORE=${ESC}8                    ## restore cursor to last saved position
TERMINAL_CU_VIS="${TERMINAL_CSI}?12l${TERMINAL_CSI}?25h" ## visible cursor
TERMINAL_CU_INVIS="${TERMINAL_CSI}?25l"        ## invisible cursor
TERMINAL_CU_NL=$TERMINAL_CU_RESTORE$TERMINAL_CU_D1$TERMINAL_CU_SAVE ## move to next line

## set attributes
TERMINAL_SET_ATTR=${TERMINAL_CSI}%sm     ## set printing attribute
TERMINAL_SET_BOLD=${TERMINAL_CSI}1m      ## equiv: printf "$TERMINAL_SET_ATTR" $TERMINAL_BOLD
TERMINAL_SET_UL=${TERMINAL_CSI}4m        ## equiv: printf "$TERMINAL_SET_ATTR" $TERMINAL_UNDERLINE
TERMINAL_SET_REV=${TERMINAL_CSI}7m       ## equiv: printf "$TERMINAL_SET_ATTR" $TERMINAL_REVERSE

## unset attributes
TERMINAL_UNSET_BOLD=${TERMINAL_CSI}22m
TERMINAL_UNSET_UL=${TERMINAL_CSI}24m
TERMINAL_UNSET_REV=${TERMINAL_CSI}27m

## colors (precede by 3 for foreground, 4 for background)
TERMINAL_BLACK=0
TERMINAL_RED=1
TERMINAL_GREEN=2
TERMINAL_YELLOW=3
TERMINAL_BLUE=4#
TERMINAL_MAGENTA=5
TERMINAL_CYAN=6
TERMINAL_WHITE=7
TERMINAL_FG=3
TERMINAL_BG=4

## set colors
TERMINAL_SET_BG="${TERMINAL_CSI}4%dm"       ## e.g.: printf "$TERMINAL_SET_BG" $TERMINAL_RED
TERMINAL_SET_FG="${TERMINAL_CSI}3%dm"       ## e.g.: printf "$TERMINAL_SET_FG" $TERMINAL_YELLOW
TERMINAL_SET_FGBG="${TERMINAL_CSI}3%d;4%dm" ## e.g.: printf "$TERMINAL_SET_FGBG" $TERMINAL_YELLOW $TERMINAL_RED



### funcs

## terminal_set_attr
# terminal_set_attr [ATTRIBUTE[;ATTRIBUTE]]
# ex : terminal_set_attr 0 $TERMINAL_BOLD $TERMINAL_UNDERLINE
terminal_set_attr()
{
    for terminal_set_attr__attr in "${@:-0}"
    do
	case $terminal_set_attr__attr in
	    ## if argument is negative, remove the attribute
	    -"$TERMINAL_BOLD") printf "$TERMINAL_SET_ATTR" "22" ;; ## 21 may not work
	    -[0-9]*) printf "$TERMINAL_SET_ATTR" "2${terminal_set_attr__attr#-}" ;;
	    ## positive number; add an attribute
	    [0-9]*|"") printf "$TERMINAL_SET_ATTR" "${terminal_set_attr__attr:-0}" ;;
	esac
    done
}


## terminal_set_fg - set the foreground (printing) color
# terminal_set_fg [0-7]
terminal_set_fg()
{
    case $1 in
	[0-7]) printf "$TERMINAL_SET_FG" "$1" ;;
	-1|"") printf "$TERMINAL_NA" ;;
	*) return 5 ;;
    esac
}


## terminal_set_bg - set the background color
# terminal_set_bg [0-7]
terminal_set_bg()
{
    case $1 in
	[0-7]) printf "$TERMINAL_SET_BG" "$1" ;;
	-1|"") printf "$TERMINAL_NA" ;;
	*) return 5 ;;
    esac
}


## terminal_set_fgbg - set the foreground and background colors
# terminal_set_fgbg [0-7] [0-7]
terminal_set_fgbg()
{
    case $1$2 in
	[0-7][0-7]) printf "$TERMINAL_SET_FGBG" "$1" "$2" ;;
	*-1*|"") printf "$TERMINAL_NA" ;;
	*) return 5 ;;
    esac
}


## terminal_cls - clear the screen
# terminal_cls
terminal_cls()
{
    printf "${TERMINAL_CLS:=$(clear)}"
}


## terminal_colors_info - display colors info
terminal_colors_info()
{
    printf "colors count : %s\n" "$(tput colors)"

    color=0
    while [ $color -lt 256 ]; do
        tput setab $color
        echo -n " $color "
        color=$(($color + 1))
    done
    tput sgr0

    # xterm
    #cd /usr/share/terminfo/x && ls xterm*color*
}
