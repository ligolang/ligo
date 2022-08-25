(* Interfacing the PascaLIGO preprocessor. *)

module File     = Preprocessing_pascaligo.File
module Comments = Preprocessing_pascaligo.Comments
module Modules  = Preprocessing_pascaligo.Modules

include Preprocessing_shared.Common.Make (File) (Comments) (Modules)
