let 
  default = import ./. {};
  inherit (default) project;
in
[
  project.pipes-ghc-events.components.library
  project.eventlog2speedscope.components.exes.eventlog2speedscope
  (import ./shell.nix {})
]
