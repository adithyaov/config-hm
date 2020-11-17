{ pkgs } :
let

  hindent-composewell =
    let src = builtins.fetchGit {
          url = "https://github.com/adithyaov/hindent.git";
          ref = "composewell-style";
        };
    in
      pkgs.haskellPackages.callPackage src {};

  ihp =
    let src =
          builtins.fetchTarball {
            url = "https://ihp.digitallyinduced.com/ihp-new.tar.gz";
          };
    in src;

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
    ihp
    hasktags
  ];
}
