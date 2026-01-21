#!/bin/sh

for option; do
    # server: -s
    if tmux show-options -s | grep -F "$option"; then
        echo "server"
    fi

    # session: -g
    if tmux show-options -g | grep -F "$option"; then
        echo "session"
    fi

    # window: -w
    if tmux show-options -w | grep -F "$option"; then
        echo "window"
    fi

    # pane: -p
    if tmux show-options -p | grep -F "$option"; then
        echo "pane"
    fi
done
