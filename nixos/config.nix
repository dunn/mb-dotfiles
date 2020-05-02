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
        bashCompletion
        editorconfig-core-c
        firefox
        gcc
        getmail
        gnumake
        gnupg
        inconsolata
        jdk
        libxml2
        libxslt
        pass
        pinentry
        pkgconfig
        postgresql
        silver-searcher
        slack
        source-code-pro
        sqlite
        tig
        tmux
        xclip
        youtube-dl
        zoom-us
      ];
    };
  };
}
