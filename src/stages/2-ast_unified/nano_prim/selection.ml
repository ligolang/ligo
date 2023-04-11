type 'expr t =
  | FieldName of Ligo_prim.Label.t
  | Component_num of (string * Simple_utils.Z.t)
  | Component_expr of 'expr
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

let get_field_name = function
  | FieldName x -> Some x
  | _ -> None


let fold_map _ _ _ = assert false
let pp _ _ _ = ()
