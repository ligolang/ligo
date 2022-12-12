(* Driving the standalone preprocessor for ReasonLIGO *)

module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module Config         = Preprocessing_reasonligo.Config
module PreprocMain    = PreprocMainGen.Make (Config)

let () = PreprocMain.run ()
