{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "ca.srid.slownews";
  android.displayName = "SlowNews";
  ios.bundleIdentifier = "ca.srid.slownews";
  ios.bundleName = "SlowNews";
})
