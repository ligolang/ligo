type 'dcl t =
  | M_struct of 'dcl list
  | M_variable of Var.Module_var.t
  | M_module_path of module_path
(* FUTURE: Functor ; Apply *)
[@@deriving eq, compare, yojson, hash, fold, map]

and module_path = Var.Module_var.t Simple_utils.List.Ne.t

let pp_module_path ppf (path : module_path) =
  Simple_utils.PP_helpers.(ne_list_sep Var.Module_var.pp (tag ".")) ppf path


let pp h ppf = function
  | M_struct p ->
    Format.fprintf
      ppf
      "@[<v>struct@,%a@,end@]"
      Simple_utils.PP_helpers.(list_sep h (tag "@,"))
      p
  | M_variable x -> Var.Module_var.pp ppf x
  | M_module_path path ->
    Simple_utils.PP_helpers.(ne_list_sep Var.Module_var.pp (tag ".")) ppf path
