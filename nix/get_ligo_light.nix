{ lib, stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "ligo";
  version = "0.54.1";

  executable = fetchurl {
    name = "ligo";
    url = "https://gitlab.com/ligolang/ligo/-/jobs/3245121952/artifacts/raw/ligo";
    sha256 = "sha256-sO8LzN4xZXVs5bNgTpnzHLQVpm9va5M6BqbeFxOaBaw=";
    executable = true;
  };

  phases = [ "installPhase" ]; # Removes all phases except installPhase

  installPhase = "
    mkdir -p $out/bin
    cp ${executable} $out/bin/ligo
  ";
}
