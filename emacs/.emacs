#!/usr/bin/env bash

alias ebug="$(which emacs) --debug-init"
alias gomacs="$(which emacs) --daemon"
alias emcs="$(which emacsclient) -c -a emacs"
alias emac="$(which emacsclient) -t -a emacs"
alias emcas=emcs
alias emca=emac
alias semac="sudo $(which emacsclient) -t -a emacs"
alias semacs="sudo $(which emacsclient) -c -a emacs"
alias boop="$(which emacsclient) -c . &"
