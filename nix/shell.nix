{ pkgs, michelson-of-ocaml }:

with pkgs;
with ocaml-ng.ocamlPackages_5_3;
mkShell {
  inputsFrom = [ michelson-of-ocaml ];
  packages = [
    nixfmt
    ocamlformat
    ocaml
    dune_3
    ocaml-lsp
    utop
  ];
}
