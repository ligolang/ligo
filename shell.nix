{ pkgs ? import ./nix/pkgs.nix { }, emacs ? false }:

with pkgs;

let
  emacsAttrs = lib.optionalAttrs emacs
    (import ./tools/instant-editor/emacs.nix { inherit pkgs; });
in mkShell ({
  inputsFrom = [ pkgs.ocamlPackages.ligo-out ];

  UTOP_SITE_LISP = "${ocamlPackages.utop}/share/emacs/site-lisp";
  MERLIN_SITE_LISP = "${ocamlPackages.merlin}/share/emacs/site-lisp";
  OCP_INDENT_SITE_LISP = "${ocamlPackages.ocp-indent}/share/emacs/site-lisp";
} // emacsAttrs)
