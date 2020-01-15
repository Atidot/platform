#!/bin/bash

DOCKERDIR="$(mktemp -d -t package-deploy)"

declare -i index=1
for var in "$@"
do
    platform-packaging pytodocker --indir "$var" --outfile "$DOCKERDIR$index"
    platform-deployment deploypython --pydir "$var" --dockerfile "$DOCKERDIR$index"
    index=$((index + 1))
done

rm -rf "$DOCKERDIR"
