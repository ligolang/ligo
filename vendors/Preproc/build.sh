#!/bin/sh
set -x
ocamllex.opt E_Lexer.mll
ocamllex.opt Preproc.mll
menhir -la 1 E_Parser.mly
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c EvalOpt.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c E_AST.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c E_Parser.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package simple-utils -c E_Lexer.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package simple-utils -c E_LexerMain.ml
camlcmd="ocamlfind ocamlc -I _x86_64 -strict-sequence -w +A-48-4   "
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package getopt,str -c EvalOpt.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package simple-utils -c E_Lexer.ml
menhir --infer --ocamlc="$camlcmd" E_Parser.mly
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c E_Parser.ml
ocamlfind ocamlc -package getopt,simple-utils,str -linkpkg -o E_LexerMain.byte E_AST.cmo E_Parser.cmo E_Lexer.cmo EvalOpt.cmo E_LexerMain.cmo
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package simple-utils -c Preproc.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package simple-utils -c PreprocMain.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package simple-utils -c Preproc.ml
ocamlfind ocamlc -package getopt,simple-utils,str -linkpkg -o PreprocMain.byte EvalOpt.cmo E_AST.cmo E_Parser.cmo E_Lexer.cmo Preproc.cmo PreprocMain.cmo
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package simple-utils -c E_ParserMain.ml
ocamlfind ocamlc -package getopt,simple-utils,str -linkpkg -o E_ParserMain.byte E_AST.cmo E_Parser.cmo E_Lexer.cmo EvalOpt.cmo Preproc.cmo E_ParserMain.cmo
