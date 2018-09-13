{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "ca.srid.slownews";
  android.displayName = "SlowNews";
  ios.bundleIdentifier = "ca.srid.slownews";
  ios.bundleName = "SlowNews";

  packages = {
    clay = pkgs.fetchFromGitHub {
      owner = "sebastiaanvisser";
      repo = "clay";
      rev = "54dc9eaf0abd180ef9e35d97313062d99a02ee75";
      sha256 = "0y38hyd2gvr7lrbxkrjwg4h0077a54m7gxlvm9s4kk0995z1ncax";
    };
  };

  overrides = self: super: with pkgs.haskell.lib; {
    Glob = dontCheck super.Glob;
    aeson-casing = dontCheck super.aeson-casing;
    clay = dontCheck super.clay;
    cryptohash-md5 = dontCheck super.cryptohash-md5;
    cryptohash-sha1 = dontCheck super.cryptohash-sha1;
    http2 = dontCheck super.http2;
    iproute = dontCheck super.iproute;
    mockery = dontCheck super.mockery;
    simple-sendfile = doJailbreak super.simple-sendfile;
    wai-app-static = dontCheck super.wai-app-static;
    wai-extra = dontCheck super.wai-extra;
    word8 = dontCheck super.word8;
  };
})
