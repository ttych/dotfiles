#!/bin/sh

# for i in $(seq 0 255) ; do
#     /bin/printf "\x1b[38;5;${i}mcolor${i} "
# done
# printf "\n"

for i in $(seq 0 255) ; do
    /bin/printf "\x1b[38;5;${i}mcolor%-5i\x1b[0m" $i
    if [  $((($i + 1) % 8)) -eq 0 ]; then
        /bin/printf '\n'
    fi
done
printf '\n'
