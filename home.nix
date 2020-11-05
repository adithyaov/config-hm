{ config, pkgs }:
let
  lib = import <nixpkgs/lib>;
  merge = import ./merge.nix { inherit lib; };
  base = {
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "adithya";
    home.homeDirectory = "/home/adithya";

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "20.09";

    # Update font cache
    fonts.fontconfig.enable = true;

    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
      hello
      cacert
      tree
      openssh
      perl532Packages.NetOpenSSH
      xcape
      xorg.xmodmap
      discord
      firefox
      xterm
      gcc
      direnv
      graphviz
      act
      docker
      cachix
      fira-code
      fira-code-symbols
    ];

    xresources.properties = {
      "XTerm*faceName" = "dejavu sans mono";
      "XTerm*background" = "#242837";
      "XTerm*foreground" = "white";
      "xterm*faceSize" = "14";
    };

  };
  git = {
    programs.git.enable = true;
    programs.git.userName = "adithyaov";
    programs.git.userEmail = "adi.obilisetty@gmail.com";
  };
  dotfiles = import ./dotfiles;
  emacs = import ./emacs { inherit pkgs; };
  haskell = import ./haskell.nix { inherit pkgs; };
  rust = import ./rust.nix { inherit pkgs; };
  node = import ./node.nix { inherit pkgs; };
  elm = import ./elm.nix { inherit pkgs; };
  python = import ./python.nix { inherit pkgs; };
in
merge [base dotfiles emacs haskell git rust node elm python]
