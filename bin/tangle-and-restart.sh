#!/bin/bash

pkill emacs
rm -rf ./build~

## Provided by doom emacs in ./doom/bin/
org-tangle "./config.org"
cp ./build~/* .
doom sync
cd /tmp && emacsclient -n -c
