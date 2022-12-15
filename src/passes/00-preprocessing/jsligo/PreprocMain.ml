(* Driving the standalone preprocessor for JsLIGO *)

module Config      = Preprocessing_jsligo.Config
module PreprocMain = Preprocessing_shared.PreprocMain.Make (Config)

let () = PreprocMain.run ()
