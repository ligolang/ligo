module PP_helpers = Simple_utils.PP_helpers
module Ne_list = Simple_utils.Ne_list
module Module_var = Var.Module_var

type 'dcl t =
  | M_struct of 'dcl list
  | M_variable of Module_var.t
  | M_module_path of module_path
(* FUTURE: Functor ; Apply *)
[@@deriving eq, compare, yojson, hash, fold, map]

and module_path = Module_var.t Ne_list.t

let pp_module_path ppf (path : module_path) =
  PP_helpers.(ne_list_sep Module_var.pp (tag ".")) ppf path


let pp h ppf = function
  | M_struct p ->
    Format.fprintf ppf "@[<v>struct@,%a@,end@]" PP_helpers.(list_sep h (tag "@,")) p
  | M_variable x -> Module_var.pp ppf x
  | M_module_path path -> PP_helpers.(ne_list_sep Module_var.pp (tag ".")) ppf path
