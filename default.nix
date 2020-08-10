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
          ;
        config =
          (nixpkgsArgs.config or {})
          ;
      };
    in import haskell-nix.sources.nixpkgs-2003 args;
  pkgs = nixpkgs;
  project =
    pkgs.haskell-nix.cabalProject {
      src = pkgs.haskell-nix.haskellLib.cleanGit { name = "pipes-ghc-events"; src = ./.; };
      compiler-nix-name = "ghc8101";
    };
  shell = import ./shell.nix { inherit default; };
  default =
    {
      inherit pkgs project;
      cache = [
        project.roots
        (pkgs.haskell-nix.withInputs shell)
      ];
    };

in default
