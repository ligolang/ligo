type 'a t =
  { module_path : Var.Module_var.t list
  ; element : 'a
  }
[@@deriving eq, compare, yojson, hash]

let pp f ppf { module_path; element } =
  match module_path with
  | [] -> Format.fprintf ppf "%a" f element
  | _ ->
    Format.fprintf
      ppf
      "%a.%a"
      Simple_utils.PP_helpers.(list_sep Var.Module_var.pp (const "."))
      module_path
      f
      element


let make_el element = { module_path = []; element }
let make module_path element = { module_path; element }
let get_el { module_path = _; element } = element
let get_path { module_path; element = _ } = module_path
