with (import <nixpkgs> {});
{
  inherit bashCompletion gnupg20 git notmuch jdk sbcl silver-searcher tmux tig nodejs-6_x;

  emacs25 = pkgs.emacs25.override {
    withGTK2 = false;
    withGTK3 = true;
    withXwidgets = true;
  };
}
