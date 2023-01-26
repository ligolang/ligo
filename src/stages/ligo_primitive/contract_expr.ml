open Var
module PP_helpers = Simple_utils.PP_helpers

type 'decl t =
  | C_struct of 'decl list
  | C_variable of Contract_var.t
  | C_module_path of Contract_var.t Module_access.t
[@@deriving equal, compare, yojson, hash, fold, map]

let pp pp_decl ppf t =
  let open PP_helpers in
  match t with
  | C_struct decls ->
    Format.fprintf ppf "@[<v>struct@,%a@,end@]" (list_sep pp_decl (tag "@,")) decls
  | C_variable x -> Contract_var.pp ppf x
  | C_module_path path -> Module_access.pp Contract_var.pp ppf path
