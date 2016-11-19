# https://nixos.org/wiki/FAQ#How_can_I_manage_software_with_nix-env_like_with_configuration.nix.3F
with (import <nixpkgs> {});
{
  inherit ansible2
          bashCompletion
          firefox
          getmail
          git
          gnupg20
          jdk
          msmtp
          nodejs-6_x
          notmuch
          sbcl
          silver-searcher
          slack
          tig
          tmux;

  emacs25 = pkgs.emacs25.override {
    withGTK2 = false;
    withGTK3 = true;
    withXwidgets = true;
  };
}
