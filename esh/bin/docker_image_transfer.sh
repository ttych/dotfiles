#!/bin/sh

from="$1"
to="$2"

echo "transfer docker image :"
echo "$from  ==>  $to"

docker pull "$from" &&
    docker "$from" "$to"&&
    docker push "$to"
