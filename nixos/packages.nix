with (import <nixpkgs> {});
{
  inherit bashCompletion gnupg20 git notmuch openjdk silver-searcher tmux tig nodejs-6_x;

  emacs25 = pkgs.emacs25.override {
    withGTK2 = false;
    withGTK3 = true;
    withXwidgets = true;
  };
}