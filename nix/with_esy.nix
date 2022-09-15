{ pkgs, lib, stdenv, esy }:

let
  ligo = pkgs.callPackage ./get_ligo.nix { };
in

stdenv.mkDerivation {
  inherit (ligo) name version;

  nativeBuildInputs = [ pkgs.makeWrapper ];

  phases = [ "installPhase" ]; # Removes all phases except installPhase

  installPhase = ''
     makeWrapper "${ligo}/bin/ligo" "$out/bin/ligo" \
      --prefix PATH : ${lib.makeBinPath (with pkgs; [ esy ])}
  ''; 
}