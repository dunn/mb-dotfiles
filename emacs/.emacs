#!/usr/bin/env bash

alias ebug='$(which emacs) --debug-init'
alias gomacs='$(which emacs) --daemon'
alias emac="emacsclient -t -a emacs"
alias emacs="emacsclient -c -a emacs"
alias emcas=emacs # i can't type
alias semac="sudo emacsclient -t -a emacs"
alias semacs="sudo emacsclient -c -a emacs"
alias boop='emacs . &'

fleemacs(){
  kill -3 "$(ps aux | ag emac.*daemon | awk '{ print $2 }' | sort -n | head -1)"
}
