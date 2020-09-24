{ config, pkgs }:
let
  lib = import <nixpkgs/lib>;
  merge = import ./merge.nix { inherit lib; };
  base = {
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "adithyaov";
    home.homeDirectory = "/home/adithyaov";

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "20.09";

    home.packages = with pkgs; [
      hello
      cacert
      tree
      openssh
      perl532Packages.NetOpenSSH
      xcape
      xorg.xmodmap
    ];

  };
  git = {
    programs.git.enable = true;
    programs.git.userName = "adithyaov";
    programs.git.userEmail = "adi.obilisetty@gmail.com";
  };
  dotfiles = import ./dotfiles;
  emacs = import ./emacs { inherit pkgs; };
  haskell = import ./haskell.nix { inherit pkgs; };
in
merge [base dotfiles emacs haskell git]
