#!/bin/bash

kill emacs
rm -rf ./build~

## Provided by doom emacs in ./doom/bin/
org-tangle "./config.org"
cp ./build~/* .
doom sync
emacsclient -n -c
