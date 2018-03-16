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

  overrides = self: super: {
    semantic-reflex =
      let src = pkgs.fetchFromGitHub {
            owner = "tomsmalley";
            repo = "semantic-reflex";
            rev = "d01e13fcf651d9f76c7003448a26659c986183e7";
            sha256 = "0d3hh9ws7nc081w7i7zsiddrx60sc69yq3sq2zsayg2v152wipkn";
          };
      in self.callCabal2nix "semantic-reflex" "${src}/semantic-reflex" {};
    };
})
