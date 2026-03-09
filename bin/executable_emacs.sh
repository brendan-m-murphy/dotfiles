#!/bin/bash
getopts "nw" OPTION
if [ "$OPTION" == 'n' ] || [ "$OPTION" == 'w' ]
then
    exec $(brew --prefix)/opt/emacs-mac/bin/emacs "$@"
else
    exec $(brew --prefix)/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs.sh "$@" &
fi
