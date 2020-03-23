(* This is the entry point of the C# preprocessor. See [Makefile.cfg]. *)

let options = EvalOpt.read ()

let () = Preproc.trace options
