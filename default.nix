{}:

let
  sources = import ./nix/sources.nix;
  haskell-nix = import sources."haskell.nix" {};
  pkgs =
    let args = haskell-nix.nixpkgsArgs; in
    import haskell-nix.sources.nixpkgs-2009 args;

  project =
    pkgs.haskell-nix.cabalProject {
      src = pkgs.haskell-nix.haskellLib.cleanGit { name = "pipes-ghc-events"; src = ./.; };
      inherit (default) compiler-nix-name index-state;
    };

  default =
    {
      inherit pkgs project;

      # Change the compiler when updating our own resolver.
      compiler-nix-name = "ghc8104";
      index-state = "2021-02-09T00:00:00Z";
    };

in default
