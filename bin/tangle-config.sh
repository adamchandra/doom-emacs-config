#!/bin/bash

emacs-28.0.50 --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "./config.org")'

# doom sync
