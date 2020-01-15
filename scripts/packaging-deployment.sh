#!/bin/bash

DEPLOYMENT="$(mktemp -d -t deployment)"

declare -i index=1
for var in "$@"
do
    platform-packaging pytodocker --input $var --outfile $DEPLOYMENT/$var
    platform-deployment _ --pythondir $var --dockerifle $DEPLOYMENT/$var # change this to work with platform-deployment
done

rm -rf "$DEPLOYMENT"
