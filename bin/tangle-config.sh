#!/bin/bash

## Provided by doom emacs in ./doom/bin/
rm -rf ./build~
org-tangle "./config.org"
cp ./build~/* .
doom sync
