{
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
  };

  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        autobuild
        autoconf
        autogen
        automake
        bashCompletion
        bind
        clang
        editorconfig-core-c
        emacs
        firefox
        gdbm
        getmail
        git
        gitAndTools.pass-git-helper
        git-secrets
        gnumake
        gnupg
        inconsolata
        jdk
        libxml2
        libxslt
        lsof
        openssl
        pass
        pinentry
        pkg-config
        pkgconfig
        postgresql
        readline
        silver-searcher
        slack
        source-code-pro
        sqlite
        tig
        tmux
        xclip
        youtube-dl
        zlib
        zoom-us
      ];
    };
  };
}
