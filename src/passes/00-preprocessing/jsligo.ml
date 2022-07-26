(* Interfacing the JsLIGO preprocessor. *)

module File     = Preprocessing_jsligo.File
module Comments = Preprocessing_jsligo.Comments
module Modules  = Preprocessing_jsligo.Modules

include Preprocessing_shared.Common.Make (File) (Comments) (Modules)
