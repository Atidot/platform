#!/bin/bash

DEPLOYMENT="$(mktemp -d -t deployment)"

declare -i index=1
for var in "$@"
do
<<<<<<< HEAD
    platform-packaging pytodocker --input $var --outfile $DEPLOYMENT/docker$index
    platform-deployment _ --pythondir $var --dockerfile $DEPLOYMENT/docker$index # change this to work with platform-deployment
    index=$(( index + 1 ))
=======
    platform-packaging pytodocker --input $var --outfile $DEPLOYMENT/$var
    packaging-deployment _ --pythondir $var --dockerifle $DEPLOYMENT/$var # change this to work with platform-deployment
>>>>>>> 0ec43910753a8703ac1964955b3e3ef9f2a7c4d4
done
