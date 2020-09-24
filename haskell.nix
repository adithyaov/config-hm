{ pkgs } :
let

  hindent-composewell = builtins.fetchGit {
    url = "git@github.com:adithyaov/hindent.git";
    ref = "composewell-style";
  };

in

{
  home.packages = with pkgs.haskellPackages; with pkgs; [
    cabal2nix
    ghc
    cabal-install
    ghcid
    hindent-composewell
  ];
}
