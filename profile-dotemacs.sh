#!/usr/bin/env bash

# documentation at:
# - https://www.emacswiki.org/emacs/ProfileDotEmacs
# source code from:
# - https://raw.githubusercontent.com/abo-abo/profile-dotemacs/master/profile-dotemacs.el

EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"

$EMACS -Q -l profile-dotemacs.el -f profile-dotemacs
