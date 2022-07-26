(* Driving the preprocessor for CameLIGO *)

module Comments       = Preprocessing_cameligo.Comments
module Modules        = Preprocessing_cameligo.Modules
module File           = Preprocessing_cameligo.File
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (Modules) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
