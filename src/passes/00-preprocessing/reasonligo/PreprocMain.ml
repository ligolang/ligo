(* Driving the preprocessor for ReasonLIGO *)

module Comments       = Preprocessing_reasonligo.Comments
module Modules        = Preprocessing_reasonligo.Modules
module File           = Preprocessing_reasonligo.File
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (Modules) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
