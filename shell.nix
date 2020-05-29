{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    opam
    dune
    ocaml
    m4
    pkgconfig
    gmp
    libev
    hidapi
    rakudo
    ocamlPackages.merlin
  ];
  UTOP_SITE_LISP = "${ocamlPackages.utop}/share/emacs/site-lisp";
  MERLIN_SITE_LISP = "${ocamlPackages.merlin}/share/emacs/site-lisp";
  OCP_INDENT_SITE_LISP = "${ocamlPackages.ocp-indent}/share/emacs/site-lisp";
}
