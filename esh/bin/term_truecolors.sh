#!/bin/bash
#---
# Excerpted from "tmux 3",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/bhtmux3 for more book information.
#---

printf "Color test:\n\n"

# Display the color bar
for colnum in {0..76}; do
    r=$((255 - (colnum * 255 / 76)))
    g=$((colnum * 510 / 76))
    b=$((colnum * 255 / 76))
    if (( g > 255 )); then
        g=$((510 - g))
    fi
    printf "\033[48;2;%d;%d;%dm" $r $g $b
    printf "\033[38;2;%d;%d;%dm" $((255 - r)) $((255 - g)) $((255 - b))
    printf " \033[0m"
done

printf "\n\nSome RGB color codes:\n\n"

for R in {0,32,64,128,192,255}; do
  for G in {0,32,64,128,192,255}; do
    for B in {0,32,64,128,192,255}; do
      printf "\033[48;2;%d;%d;%dm%3d,%3d,%3d\033[0m " \
        "$R" "$G" "$B" "$R" "$G" "$B"
    done
    echo
  done
  echo
done
