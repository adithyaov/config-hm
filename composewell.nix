{pkgs}:

let
  dev-tools =
    builtins.fetchGit {
      url = "git@github.com:composewell/dev-tools.git";
      rev = "47fd8b8eb2df6b55eed460b13dcc1c56da2a7c1a";
    };
  git-cabal =
    let file = builtins.readFile "${dev-tools}/bin/git-cabal";
    in pkgs.writeShellScriptBin "git-cabal" file;
  core-2-core-diff =
    let file = builtins.readFile "${dev-tools}/bin/core-2-core-diff";
    in pkgs.writeShellScriptBin "core-2-core-diff" file;
in
{
  home.packages = [git-cabal core-2-core-diff];
}
