#!/usr/bin/env bash

# Install command-line tools using Homebrew.

# Make sure we’re using the latest Homebrew.
brew update

# Upgrade any already-installed formulae.
brew upgrade

# If it isn't already installed
brew reinstall git --with-persistent-https --with-brewed-openssl --with-pcre --with-brewed-curl
brew install hub

# Install GNU core utilities (those that come with OS X are outdated).
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
brew install coreutils
brew install gnu-sed
brew install gawk

# Install Bash 4.
# Note: don’t forget to add `/usr/local/bin/bash` to `/etc/shells` before
# running `chsh`.
brew install bash
brew tap homebrew/versions
brew install bash-completion2

# Install emacs
brew reinstall emacs --with-x
brew reinstall aspell --with-lang-en

# Install wget with IRI support.
brew reinstall wget --with-iri

# Install curl with sftp support, necessary for git-ftp
brew reinstall curl --with-libssh2 --with-openssl

# Install MAMP stuff
brew reinstall mysql --enable-local-infile
brew tap homebrew/apache
brew reinstall httpd24 --with-homebrew-openssl
brew tap homebrew/php
brew reinstall php56 --with-apache --homebrew-apxs --with-homebrew-openssl --with-homebrew-curl

# Email
brew reinstall mutt --with-confirm-attachment-patch --with-gpgme --with-s-lang --with-trash-patch
brew install getmail
brew install msmtp
brew install gpg-agent

# Install other good shit
brew install ag
brew install ack
brew install cheat
brew install editorconfig
brew install entr
brew install heroku-toolbelt
brew install htmlcompressor
brew reinstall imagemagick --with-x11 --with-webp
brew install pandoc
brew install pandoc-citeproc
brew install pngquant
brew install pigz
brew reinstall poppler --with-little-cms2
brew install reattach-to-user-namespace
brew install rename
brew install ruby
brew install safe-rm
brew install src
brew install surfraw
brew install tmux
brew install urlview
brew install w3m
brew install youtube-dl
brew install zopfli

# Install Node.js
brew reinstall node --with-openssl --without-icu4c
brew install nvm

# Remove outdated versions from the cellar.
brew cleanup
