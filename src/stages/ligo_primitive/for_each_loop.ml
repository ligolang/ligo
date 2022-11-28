type collect_type =
  | Map
  | Set
  | List
  | Any
[@@deriving eq, compare, yojson, hash]

let pp_collect_type ppf collect_type =
  match collect_type with
  | Map -> Format.fprintf ppf "map"
  | Set -> Format.fprintf ppf "set"
  | List -> Format.fprintf ppf "list"
  | Any -> Format.fprintf ppf "any"


type 'e t =
  { fe_binder : Var.Value_var.t * Var.Value_var.t option
  ; collection : 'e
  ; collection_type : collect_type
  ; fe_body : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let option_map ppf (k, v_opt) =
  match v_opt with
  | None -> Format.fprintf ppf "%a" Var.Value_var.pp k
  | Some v -> Format.fprintf ppf "%a -> %a" Var.Value_var.pp k Var.Value_var.pp v


let pp f ppf { fe_binder; collection; fe_body; _ } =
  Format.fprintf ppf "for each %a in %a do %a" option_map fe_binder f collection f fe_body


let fold_map f acc { fe_binder; collection; fe_body; collection_type } =
  let acc, collection = f acc collection in
  let acc, fe_body = f acc fe_body in
  acc, { fe_binder; collection; fe_body; collection_type }
