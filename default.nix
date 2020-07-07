{}:

let
  sources = import ./nix/sources.nix;
  haskell-nix = import sources."haskell.nix" {};
  nixpkgs =
    let
      inherit (haskell-nix) nixpkgsArgs;
      args = nixpkgsArgs // {
        overlays =
          (nixpkgsArgs.overlays or [])
          ++ [ (import ./nix/ghcide.nix { inherit sources; }) ]
          ;
        config =
          (nixpkgsArgs.config or {})
          ;
      };
    in import haskell-nix.sources.nixpkgs-1909 args;
  pkgs = nixpkgs;
  project =
    pkgs.haskell-nix.project {
      src = pkgs.haskell-nix.haskellLib.cleanGit { name = "pipes-ghc-events"; src = ./.; };
    };
  shell = import ./shell.nix { inherit default; };
  default =
    {
      inherit pkgs project;
      cache = [
        pkgs.haskell-nix.haskellNixRoots
        (pkgs.haskell-nix.withInputs shell)
      ];
    };

in default
