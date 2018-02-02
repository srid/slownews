# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  android.frontend = {
    executableName = "slownews-frontend";
    applicationId = "ca.srid.slownews";
    displayName = "SlowNews";
  };

  ios.frontend = {
    executableName = "slownews-frontend";
    bundleIdentifier = "ca.srid.slownews";
    bundleName = "SlowNews";
  };
})
