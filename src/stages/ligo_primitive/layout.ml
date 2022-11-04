open Var

type t =
  | L_comb
  | L_tree
  | L_variable of Layout_var.t
[@@deriving eq, compare, yojson, hash]

(* External view for layouts. Used post-typer. This hack is used since parameterising the type
   in [Ast_typed] for layout doesn't work with ppx_woo (or ppx_import)  *)
type view =
  | L_comb
  | L_tree
[@@deriving eq, compare, yojson, hash]

let view (t : t) : view =
  match t with
  | L_comb -> L_comb
  | L_tree -> L_tree
  | L_variable _lvar -> failwith "corner case: cannot view a layout variable"


let pp ppf (t : t) =
  match t with
  | L_tree -> Format.fprintf ppf "tree"
  | L_comb -> Format.fprintf ppf "comb"
  | L_variable lvar -> Format.fprintf ppf "^%a" Layout_var.pp lvar
