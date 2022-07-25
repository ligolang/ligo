(* Interfacing the CameLIGO preprocessor. *)

module File     = Preprocessing_cameligo.File
module Comments = Preprocessing_cameligo.Comments
module Modules  = Preprocessing_cameligo.Modules

include Preprocessing_shared.Common.Make (File) (Comments) (Modules)
