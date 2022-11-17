(* Driving the standalone preprocessor for CameLIGO *)

module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module Config         = Preprocessing_cameligo.Config
module PreprocMain    = PreprocMainGen.Make (Config)

let () = PreprocMain.run ()
