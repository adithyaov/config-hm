{ pkgs } :
let

  hindent-composewell =
    let src = builtins.fetchGit {
          url = "https://github.com/adithyaov/hindent.git";
          ref = "composewell-style";
        };
    in
      pkgs.haskellPackages.callPackage src {};

in

{
  home.packages = with pkgs.haskellPackages; with pkgs; [
    cabal2nix
    ghc
    cabal-install
    ghcid
    hindent-composewell
    hlint
    blaze-from-html
    haskell-language-server
  ];
}
