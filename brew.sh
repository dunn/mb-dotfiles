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

# Install Bash 4.
# Note: don’t forget to add `/usr/local/bin/bash` to `/etc/shells` before
# running `chsh`.
brew install bash
brew install bash-completion

# Install emacs
brew install emacs --with-x
brew install aspell --with-lang-en

# Install wget with IRI support.
brew install wget --with-iri

# Install curl with sftp support, necessary for git-ftp
brew install curl --with-libssh2 --with-openssl

# Install MAMP stuff
brew install mysql --enable-local-infile
brew tap homebrew/apache
brew install httpd24 --with-homebrew-openssl
brew tap homebrew/php
brew install php56 --with-apache --homebrew-apxs --with-homebrew-openssl --with-homebrew-curl

# Email
brew install mutt --with-confirm-attachment-patch --with-gpgme --with-s-lang --with-trash-patch
brew install getmail
brew install msmtp
brew install gpg-agent

# Install other good shit
brew install ag
brew install ack
brew install cheat
brew install editorconfig
brew install entr
brew install gawk
brew install heroku-toolbelt
brew install htmlcompressor
brew install imagemagick --with-x11 --with-webp
brew install pandoc
brew install pandoc-citeproc
brew install pngquant
brew install pigz
brew install poppler --with-little-cms2
brew install reattach-to-user-namespace
brew install rename
brew install ruby
brew install src
brew install surfraw
brew install tmux
brew install urlview
brew install w3m
brew install youtube-dl
brew install zopfli

# Install Node.js. Note: this installs `npm` too, using the recommended
# installation method.
brew install node
brew install nvm

# Install io.js
brew install iojs

# Remove outdated versions from the cellar.
brew cleanup
