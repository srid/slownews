{ mkDerivation, aeson, base, containers, ghcjs-base, miso, stdenv
}:
mkDerivation {
  pname = "slownews-frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers ghcjs-base miso
  ];
  description = "SlowNews Frontend";
  license = stdenv.lib.licenses.unfree;
}
