{ pkgs, doCheck ? true }:

let
  inherit (pkgs) lib stdenv ocaml-ng;
  ocamlPackages = ocaml-ng.ocamlPackages_5_3;
in with ocamlPackages;
buildDunePackage {
  pname = "michelson-of-ocaml";
  version = "0.0.0-dev";

  src = ./..;

  propagatedBuildInputs = [
    menhir
    menhirLib
    sedlex
    ppx_deriving
    eio
    eio_main
    ppx_sexp_conv
    zarith
    lsp
    alcotest
  ];
}
