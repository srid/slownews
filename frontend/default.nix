{ pkgs ? import <nixpkgs> {} }:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "3ca0ca2acf0c01f1e718bbdd78371fab53a02fe2";
    sha256 = "0a6d470bar1wz6950zy3h7x2p92a68x723n44l4c7c6zs5zrasgh";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
