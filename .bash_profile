#!/usr/bin/env bash

export PATH="/usr/local/bin:/usr/local/sbin:/usr/texbin:$PATH"

export PATH="$HOME/bin:$(brew --prefix go)/libexec/bin:$PATH"

. "$HOME/.bash_prompt"
. "$HOME/.exports"
. "$HOME/.aliases"
. "$HOME/.functions"
. "$HOME/.brew"
# symlink the contents of ./emacs into your home folder, not the
# directory itself
. "$HOME/.emacs"
[[ -r "$HOME/.extra" ]] && . "$HOME/.extra"

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
if which brew > /dev/null && [[ -f "$(brew --prefix)/share/bash-completion/bash_completion" ]]; then
  . "$(brew --prefix)/share/bash-completion/bash_completion";
elif [[ -f /etc/bash_completion ]]; then
  . /etc/bash_completion;
fi;

if [[ -f "$(brew --repository)/Library/Contributions/brew_bash_completion.sh" ]]; then
  . "$(brew --repository)/Library/Contributions/brew_bash_completion.sh"
fi

# brew install awscli
complete -C aws_completer aws

# brew install autojump
[[ -s "$(brew --prefix)/etc/profile.d/autojump.sh" ]] && . "$(brew --prefix)/etc/profile.d/autojump.sh"

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;
