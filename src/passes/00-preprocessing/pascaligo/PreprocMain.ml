(* Driving the standalone preprocessor for PascaLIGO *)

module Config      = Preprocessing_pascaligo.Config
module PreprocMain = Preprocessing_shared.PreprocMain.Make (Config)

let () = PreprocMain.run ()
