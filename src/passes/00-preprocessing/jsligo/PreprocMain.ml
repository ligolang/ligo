(* Driving the preprocessor for JsLIGO *)

module Comments       = Preprocessing_jsligo.Comments
module Modules        = Preprocessing_jsligo.Modules
module File           = Preprocessing_jsligo.File
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (Modules) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
