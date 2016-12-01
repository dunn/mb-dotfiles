# https://nixos.org/wiki/FAQ#How_can_I_manage_software_with_nix-env_like_with_configuration.nix.3F
with (import <nixpkgs> {});
{
  inherit ansible2
          bashCompletion
          bundler
          firefox
          gcc
          getmail
          git
          gnumake
          gnupg20
          jdk
          libxml2
          libxslt
          msmtp
          nodejs-6_x
          notmuch
          pass
          pkgconfig
          postgresql
          ruby
          sbcl
          silver-searcher
          slack
          sqlite
          tig
          tmux
          xclip
          zoom-us;

  emacs25 = pkgs.emacs25.override {
    withGTK2 = false;
    withGTK3 = true;
    withXwidgets = true;
  };
}
