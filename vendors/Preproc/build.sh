#!/bin/sh
set -x
ocamllex.opt Escan.mll
ocamllex.opt Preproc.mll
menhir -la 1 Eparser.mly
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Etree.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Error.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Etree.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Error.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Eparser.mli
camlcmd="ocamlfind ocamlc -I _i686 -strict-sequence -w +A-48-4   "
menhir --infer --ocamlc="$camlcmd" Eparser.mly
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Escan.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Eparser.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Preproc.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Escan.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Preproc.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c EMain.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c EMain.ml
ocamlfind ocamlopt -o EMain.opt Etree.cmx Eparser.cmx Error.cmx Escan.cmx Preproc.cmx EMain.cmx
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c ProcMain.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c ProcMain.ml
ocamlfind ocamlopt -o ProcMain.opt Etree.cmx Eparser.cmx Error.cmx Escan.cmx Preproc.cmx ProcMain.cmx
