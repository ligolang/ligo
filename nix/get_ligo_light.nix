{ lib, stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "ligo";
  version = "0.52.0";

  executable = fetchurl {
    name = "ligo";
    url = "https://gitlab.com/ligolang/ligo/-/jobs/3097040721/artifacts/raw/ligo";
    sha256 = "sha256-oqgQrcn6vIIfYjwupSHLbMVMWbZgZyfQCSyRDH+mNF4=";
    executable = true;
  };

  phases = [ "installPhase" ]; # Removes all phases except installPhase

  installPhase = "
    mkdir -p $out/bin
    cp ${executable} $out/bin/ligo
  ";
}
