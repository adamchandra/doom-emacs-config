#!/bin/bash

## Provided by doom emacs in ./doom/bin/
org-tangle "./config.org"
cp ./build~/* .
doom sync
