type t =
  | L_comb
  | L_tree
[@@deriving eq, compare, yojson, hash]

let pp ppf (t : t) =
  match t with
  | L_tree -> Format.fprintf ppf "tree"
  | L_comb -> Format.fprintf ppf "comb"


let default_layout : t = L_tree
