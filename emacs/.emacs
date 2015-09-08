#!/usr/bin/env bash

alias ebug='$(which emacs) --debug-init'
alias gomacs='$(which emacs) --daemon'
alias emcs="emacsclient -c -a emacs"
alias emac="emacsclient -t -a emacs"
alias emcas=emcs
alias emca=emac
alias semac="sudo emacsclient -t -a emacs"
alias semacs="sudo emacsclient -c -a emacs"
alias boop='emacs . &'
