module Ty_escaped_var = struct
  type t =
    | Esc of Ligo_prim.Type_var.t
    | Raw of Ligo_prim.Type_var.t
  [@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
end

module Value_escaped_var = struct
  type t =
    | Esc of Ligo_prim.Value_var.t
    | Raw of Ligo_prim.Value_var.t
  [@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
end
