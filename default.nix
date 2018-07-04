{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "ca.srid.slownews";
  android.displayName = "SlowNews";
  ios.bundleIdentifier = "ca.srid.slownews";
  ios.bundleName = "SlowNews";

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
