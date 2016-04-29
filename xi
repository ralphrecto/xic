#! /bin/bash

if [ -f bin/xi.native ]; then
    bin/xi.native "$@"
else
    bin/xi.byte "$@"
fi
