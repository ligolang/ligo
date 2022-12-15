(* Driving the standalone preprocessor for PyLIGO *)

module Config      = Preprocessing_pyligo.Config
module PreprocMain = Preprocessing_shared.PreprocMain.Make (Config)

let () = PreprocMain.run ()
