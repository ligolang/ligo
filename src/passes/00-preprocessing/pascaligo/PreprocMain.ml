(* Driving the standalone preprocessor for PascaLIGO *)

module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module Config         = Preprocessing_pascaligo.Config
module PreprocMain    = PreprocMainGen.Make (Config)

let () = PreprocMain.run ()
