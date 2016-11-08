with (import <nixpkgs> {});
{
  inherit bashCompletion
          getmail
          git
          gnupg20
          jdk
          msmtp
          nodejs-6_x
          notmuch
          sbcl
          silver-searcher
          tig
          tmux;

  emacs25 = pkgs.emacs25.override {
    withGTK2 = false;
    withGTK3 = true;
    withXwidgets = true;
  };
}
