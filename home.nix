{ config, pkgs }:
let
  lib = import <nixpkgs/lib>;
  merge = import ./merge.nix { inherit lib; };

  emacsOverlay = import (builtins.fetchTarball {
    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  });

  discordOverlay =
    let url = https://discord.com/api/download?platform=linux&format=tar.gz;
    in self: super: {
      discord =
        super.discord.overrideAttrs
          (_: {
            src = builtins.fetchTarball url;
          });
    };

  haskellPackagesOverlay =
    let
      makeHaskellPkgGit =
        super: gitSrc: ref: name:
        let src =
              builtins.fetchGit {
                url = gitSrc;
                ref = ref;
              };
        in super.callCabal2nix name src {};
      makeHaskellPkgGitMaster =
        super: gitSrc: name: makeHaskellPkgGit super gitSrc "master" name;
    in
      self: super: {
        haskellPackages =
          super.haskellPackages.override {
            overrides =
              self: super: {
                hakyll =
                  let src = "https://github.com/jaspervdj/hakyll.git";
                  in makeHaskellPkgGitMaster super src "hakyll";
              };
          };
      };

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

    # Enable direnv
    programs.direnv.enable = true;
    programs.direnv.enableNixDirenvIntegration = true;

    nixpkgs.config.allowUnfree = true;

    # Many Haskell packages seem to be marked as broken!
    # nixpkgs.config.allowBroken = true;

    nixpkgs.overlays = [ emacsOverlay discordOverlay haskellPackagesOverlay ];


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
      graphviz
      act
      docker
      cachix
      fira-code
      fira-code-symbols
      nixfmt
      xdotool
      xbindkeys
      lxd
      silver-searcher
    ];

    xresources.properties = {
      "xterm*faceName" = "Fira Code";
      "xterm*faceSize" = "10";
      # special
      "xterm*foreground" = "#d7dde7";
      "xterm*background" = "#030405";
      "xterm*cursorColor" = "#d7dde7";
      # black
      "xterm*color0" = "#06070b";
      "xterm*color8" = "#fe798d";
      # red
      "xterm*color1" = "#11151b";
      "xterm*color9" = "#e045e8";
      # green
      "xterm*color2" = "#212730";
      "xterm*color10" = "#ed93f3";
      # yellow
      "xterm*color3" = "#37404d";
      "xterm*color11" = "#aa8b28";
      # blue
      "xterm*color4" = "#525f71";
      "xterm*color12" = "#b19651";
      # magenta
      "xterm*color5" = "#74859d";
      "xterm*color13" = "#68adf2";
      # cyan
      "xterm*color6" = "#a2b1c8";
      "xterm*color14" = "#f767fe";
      # white
      "xterm*color7" = "#dee3eb";
      "xterm*color15" = "#8ab7eb";
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
  composewell = import ./composewell.nix { inherit pkgs; };
in
merge [base dotfiles emacs haskell git rust node elm python composewell]
