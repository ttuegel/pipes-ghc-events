{ default ? import ./default.nix {} }:

let
  inherit (default) project;
  local =
    if builtins.pathExists ./shell.local.nix
    then import ./shell.local.nix { inherit default; }
    else x: x;
  shellFor = args: project.shellFor (local args);
in

shellFor {
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
