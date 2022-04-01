{ default ? import ./default.nix {} }:

let
  inherit (default) project;
  inherit (default.pkgs) lib;
  local =
    if builtins.pathExists ./shell.local.nix
    then import ./shell.local.nix { inherit default; }
    else x: x;
  shellFor = args: project.shellFor (local args);
  latest = args: {
    index-state = args.index-state or default.index-state;
    version = args.version or "latest";
    modules = args.modules or [];
  };
  compose = fs: z: lib.foldr (f: x: f x) z fs;
in

shellFor {
  tools = {
    cabal = latest {
      modules = [(
        { lib, ... }:
        {
          options.nonReinstallablePkgs = lib.mkOption {
            apply = compose [
              (lib.remove "Cabal")
            ];
          }; 
        }
      )];
    };
    hlint = latest {
      modules = [(
        { lib, ... }:
        {
          options.nonReinstallablePkgs = lib.mkOption {
            apply = compose [
              (xs: xs ++ ["exceptions" "stm"])
            ];
          }; 
        }
      )];
    };
    haskell-language-server = latest {
      modules = [(
        { lib, ... }:
        {
          options.nonReinstallablePkgs = lib.mkOption {
            apply = compose [
              (xs: xs ++ ["exceptions" "stm"])
              (lib.remove "Cabal")
            ];
          }; 
        }
      )];
    };
  };
}
