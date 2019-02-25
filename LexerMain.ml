(* Driver for the lexer of Ligo *)

open! EvalOpt (* Reads the command-line options: Effectful! *)

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

let external_ text =
  Utils.highlight (Printf.sprintf "External error: %s" text); exit 1;;

(* Running the lexer on the input file *)

module Lexer = Lexer.Make (LexToken)

let () = Lexer.trace ~offsets:EvalOpt.offsets
                     EvalOpt.mode EvalOpt.input EvalOpt.cmd
