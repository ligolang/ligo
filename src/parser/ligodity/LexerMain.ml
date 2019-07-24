(* Driver for the lexer of CameLIGO *)

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

(* Running the lexer on the source *)

let options = EvalOpt.read ()

open EvalOpt;;

if Utils.String.Set.mem "lexer" options.verbose then
  Lexer.trace options.input
else Lexer.iter (fun _lexbuf _out _token -> ()) options.input
;;
