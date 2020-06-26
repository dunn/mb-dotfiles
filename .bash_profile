#!/usr/bin/env bash

ulimit -n 200000
ulimit -u 2048

if [[ $(uname -s) == "Darwin" ]]; then
  export PATH="$HOME/bin:/usr/local/opt/python/libexec/bin:/usr/local/opt/node/bin:$HOME/.cargo/bin:$HOME/.rbenv/bin:$HOME/perl5/bin:/usr/local/bin:/usr/local/sbin:/usr/local/texlive/2016/bin/x86_64-darwin:$PATH"
else
  export PATH="$HOME/bin:$HOME/.cargo/bin:$HOME/.npm-packages/bin:$HOME/perl5/bin:/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/bin:/usr/bin:/usr/sbin:/bin:/sbin:$PATH"
fi

. "$HOME/.exports"
. "$HOME/.bash_prompt"
. "$HOME/.aliases"
. "$HOME/.functions"
. "$HOME/.brew"
. "$HOME/.emacs"
[[ -r "$HOME/.extra" ]] && . "$HOME/.extra"

eval "$(rbenv init -)" 2>/dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# http://mywiki.wooledge.org/BashGuide/Patterns
shopt -s extglob

# https://twitter.com/isislovecruft/status/891065653200924672
shopt -u sourcepath

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
