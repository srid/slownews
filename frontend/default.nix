{ pkgs ? import <nixpkgs> {} }:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "d280526473f787cdc653cc1adf6cc315991a058d";
    sha256 = "01y3z0ldbwhksvnr8rylcnvia74r9g1nqiix7nr0q15g0fk7gvn0";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
