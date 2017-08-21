{ mkDerivation, aeson, base, containers, ghcjs-base, miso, stdenv
, time
}:
mkDerivation {
  pname = "slownews-frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers ghcjs-base miso time
  ];
  description = "SlowNews Frontend";
  license = stdenv.lib.licenses.unfree;
}
