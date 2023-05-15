open Imports
include Lsp.Types.DocumentUri

let eq = Caml.( = )

(** Checks if uri refers to location's filepath.
  Note: this function does not normalise filepaths
  so you may want to do this on its arguments *)
let matches_loc : t -> Loc.t -> bool =
 fun uri -> function
         | File region ->
            if Sys.unix then
              eq uri (of_path region#file)
            else String.equal (to_path uri) (Str.global_replace (Str.regexp "\\\\\\\\\\\\\\\\") "\\\\" region#file)

  | Virtual _ -> false


let get_extension : t -> string option = snd <@ Filename.split_extension <@ to_path
let get_syntax = Syntax.of_ext_opt ~support_pascaligo:true <@ get_extension
