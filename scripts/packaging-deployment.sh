#!/bin/bash

DEPLOYMENT="$(mktemp -d -t deployment)"

declare -i index=1
for var in "$@"
do
    platform-packaging pytodocker --input $var --outfile $DEPLOYMENT/$var
    platform-deployment -s someScript # change this to work with platform-deployment
done

rm -rf "$DEPLOYMENT"
