(* Driving the standalone preprocessor for CameLIGO *)

module Config      = Preprocessing_cameligo.Config
module PreprocMain = Preprocessing_shared.PreprocMain.Make (Config)

let () = PreprocMain.run ()
