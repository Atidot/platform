#!/bin/bash

DEPLOYMENT="$(mktemp -d -t deployment)"

declare -i index=1
for var in "$@"
do
    platform-packaging pytodocker --input $var --outfile $DEPLOYMENT/docker$index
    platform-deployment _ --pythondir $var --dockerfile $DEPLOYMENT/docker$index # change this to work with platform-deployment
    index=$(( index + 1 ))
done
