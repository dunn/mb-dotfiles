#!/usr/bin/env bash

. "$HOME/.exports"
. "$HOME/.bash_prompt"
. "$HOME/.aliases"
. "$HOME/.functions"
. "$HOME/.brew"
# symlink the contents of ./emacs into your home folder, not the
# directory itself
. "$HOME/.emacs"
[[ -r "$HOME/.extra" ]] && . "$HOME/.extra"

if [[ $(uname -s) == "Darwin" ]]; then
  export PATH="$HOME/bin:/usr/local/opt/node/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:$PATH"
else
  export PATH="$HOME/bin:$HOME/.linuxbrew/bin:$HOME/.linuxbrew/sbin:/usr/bin:/usr/sbin:/bin:/sbin:$PATH"
fi

eval "$(rbenv init -)" 2>/dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# https://github.com/bfontaine/Dotfiles/blob/master/.bashrc#L24-L25
# disable file overwriting with >
set -C

# Autocorrect typos in path names when using `cd`
# shopt -s cdspell;

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
  shopt -s "$option" 2> /dev/null;
done;

# brew install bash-completion2
if which brew > /dev/null 2>&1 && [[ -f "$(brew --prefix bash-completion2)/share/bash-completion/bash_completion" ]]; then
  . "$(brew --prefix bash-completion2)/share/bash-completion/bash_completion"
elif [[ -f /etc/bash_completion ]]; then
  . /etc/bash_completion;
fi;

if which brew > /dev/null 2>&1 && [[ -f "$(brew --repository)/Library/Contributions/brew_bash_completion.sh" ]]; then
  . "$(brew --repository)/Library/Contributions/brew_bash_completion.sh"
fi

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;
