(* Driving the standalone preprocessor for JsLIGO *)

module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module Config         = Preprocessing_jsligo.Config
module PreprocMain    = PreprocMainGen.Make (Config)

let () = PreprocMain.run ()
