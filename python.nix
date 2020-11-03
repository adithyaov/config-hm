{ pkgs } :
{
  home.packages = with pkgs.python38Packages; [
    python
    pip
  ];
}
