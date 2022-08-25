(* Interfacing the ReasonLIGO preprocessor. *)

module File     = Preprocessing_reasonligo.File
module Comments = Preprocessing_reasonligo.Comments
module Modules  = Preprocessing_reasonligo.Modules

include Preprocessing_shared.Common.Make (File) (Comments) (Modules)
