type 'a t = {
  module_path : Var.ModuleVar.t list ;
  element     : 'a ;
}
[@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {module_path;element} ->
  Format.fprintf ppf "%a.%a"
    Simple_utils.PP_helpers.(list_sep (Var.ModuleVar.pp) (const ".")) module_path
    f element
