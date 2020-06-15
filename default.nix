{ mkDerivation, aeson, attoparsec, base, bytestring, containers, stdenv, text, unordered-containers, vector }:
mkDerivation {
  pname = "jsonwrench";
  version = "0.7.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers text unordered-containers vector
  ];
  homepage = "https://github.com/jekor/jsonwrench";
  description = "JSON building and manipulation on the commandline";
  license = stdenv.lib.licenses.mit;
}
