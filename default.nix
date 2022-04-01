{}:

let
  sources = import ./nix/sources.nix;
  haskell-nix = import sources."haskell.nix" {};
  pkgs = import haskell-nix.sources.nixpkgs-unstable haskell-nix.nixpkgsArgs;

  project =
    pkgs.haskell-nix.cabalProject {
      src = pkgs.haskell-nix.haskellLib.cleanGit { name = "pipes-ghc-events"; src = ./.; };
      inherit (default) compiler-nix-name index-state;
    };

  default =
    {
      inherit pkgs project;

      # Change the compiler when updating our own resolver.
      compiler-nix-name = "ghc902";
      index-state = "2022-03-27T00:00:00Z";
    };

in default
