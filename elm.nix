{ pkgs } :
{
  home.packages = with pkgs; with pkgs.elmPackages; [
    elm2nix
    elm
    elm-format
    create-elm-app
  ];
}
