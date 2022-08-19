type t =
  | L_comb
  | L_tree
  [@@deriving eq,compare,yojson,hash]

let pp ppf = function
  | L_tree -> Format.fprintf ppf "tree"
  | L_comb -> Format.fprintf ppf "comb"

