{ pkgs } :
{
  home.packages = with pkgs; [
    rustc
    cargo
    nushell
    tealdeer
    sd
    fd
    ripgrep
  ];
}
