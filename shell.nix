{ default ? import ./default.nix {} }:

let
  inherit (default) project pkgs compiler-nix-name;
  inherit (pkgs) cabal-install;

  local =
    if builtins.pathExists ./shell.local.nix
    then import ./shell.local.nix { inherit default; }
    else x: x;
  shellFor = args: project.shellFor (local args);

  sources = import ./nix/sources.nix;

  hls-project = import sources."nix-haskell-hls" {
    ghcVersion = compiler-nix-name;
  };
  inherit (hls-project) hls-renamed;

  hlint-project = default.pkgs.haskell-nix.cabalProject {
    src = sources."hlint";
    inherit (default) compiler-nix-name index-state;
  };
  inherit (hlint-project.hlint.components.exes) hlint;

in

shellFor {
  buildInputs = with pkgs; [
    cabal-install hlint hls-renamed
  ];
}
