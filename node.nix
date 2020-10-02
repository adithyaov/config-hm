{ pkgs } :
{
  home.packages = with pkgs; with pkgs.nodePackages; [
    nodejs
    npm
    vue-cli
  ];
}
