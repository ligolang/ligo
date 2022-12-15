(* Driving the standalone preprocessor for ReasonLIGO *)

module Config      = Preprocessing_reasonligo.Config
module PreprocMain = Preprocessing_shared.PreprocMain.Make (Config)

let () = PreprocMain.run ()
