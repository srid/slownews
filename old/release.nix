with (import <nixpkgs> {});
let
  slownews = callPackage ./default.nix {};
in
derivation {
  name = "slownews";
  builder = "${bash}/bin/bash";
  args = [ ./build.sh ];
  buildInputs = [ slownews.ghc.backend slownews.ghcjs.frontend ];
  coreutils = coreutils;
  frontend = slownews.ghcjs.frontend;
  backend = slownews.ghc.backend;
  slownewsConfig = ./backend/config;
  system = builtins.currentSystem;
}
