(* Driver for the lexer of Mini-ML *)

(* Error printing and exception tracing *)

Printexc.record_backtrace true;;

(* Running the lexer on the source *)

if Utils.String.Set.mem "lexer" EvalOpt.verbose then
  Lexer.trace EvalOpt.input
else Lexer.iter (fun _lexbuf _out _token -> ()) EvalOpt.input
;;
