#!/usr/bin/env sh

# emacsclient helpers
# --- Core emacsclient shortcut ---
alias ec='emacsclient -t -a ""'

# Open current directory (useful entry point)
alias ecd='ec .'

# Open file(s)
# usage: ecf file.py
alias ecf='ec'

# --- Start daemon explicitly (rarely needed if using -a "") ---
alias edstart='emacs --daemon'

# --- Kill daemon cleanly ---
alias edkill='emacsclient -e "(kill-emacs)"'

# --- Hard reset (use when things go weird) ---
alias edkillkill='pkill -9 -u "$USER" emacs && rm -rf /run/user/$UID/emacs'

# --- Magit ---
magit() {
  emacsclient -t -a "" -e "(magit-status \"$(pwd)\")"
}
