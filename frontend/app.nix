{ mkDerivation, base, containers, miso, stdenv }:
mkDerivation {
  pname = "slownews-frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers miso ];
  description = "SlowNews Frontend";
  license = stdenv.lib.licenses.unfree;
}
