{pkgs}:

let
  dev-tools =
    builtins.fetchGit {
      url = "git@github.com:composewell/dev-tools.git";
      ref = "master";
    };
  git-cabal =
    let file = builtins.readFile "${dev-tools}/git-cabal";
    in pkgs.writeShellScriptBin "git-cabal" file;
  core-2-core-diff =
    let file = builtins.readFile "${dev-tools}/core-2-core-diff";
    in pkgs.writeShellScriptBin "core-2-core-diff" file;
in
{
  home.packages = [git-cabal core-2-core-diff];
}
