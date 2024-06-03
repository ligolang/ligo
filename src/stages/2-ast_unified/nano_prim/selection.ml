module Label = Ligo_prim.Label
module Ligo_z = Simple_utils.Ligo_z

type 'expr t =
  | FieldName of Label.t
  | Component_num of (string * Ligo_z.t)
  | Component_expr of 'expr
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

let get_field_name = function
  | FieldName x -> Some x
  | _ -> None


let fold_map _ _ _ = assert false
let pp _ _ _ = ()
