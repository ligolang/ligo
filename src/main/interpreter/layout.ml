module AST = Ast_aggregated
module Append_tree = Simple_utils.Tree.Append
open Simple_utils.Trace
open Ligo_interpreter.Types
open Ligo_prim

let extract_record ~raise ~(layout:Layout.t) (v : value) (lst : (Label.t * AST.type_expression) list) : _ list =
  match layout with
  | L_tree -> (
    let open Append_tree in
    let tree = match Append_tree.of_list lst with
      | Empty -> raise.error @@ Errors.generic_error Location.generated "empty record"
      | Full t -> t in
    let rec aux tv : (Label.t * (value * AST.type_expression)) list =
      match tv with
      | Leaf (s, t), v -> [s, (v, t)]
      | Node {a;b;size=_;full=_}, v when Option.is_some (Ligo_interpreter.Combinators.get_pair v) ->
          let (va, vb) = trace_option ~raise (Errors.generic_error Location.generated "Expected pair") @@
                           Ligo_interpreter.Combinators.get_pair v in
          let a' = aux (a, va) in
          let b' = aux (b, vb) in
          (a' @ b')
      | _ -> raise.error @@ Errors.generic_error Location.generated "bad record path"
    in
    aux (tree, v)
  )
  | L_comb -> (
    let rec aux lst_record v : (Label.t * (value * AST.type_expression)) list =
      match lst_record,v with
      | [], _ -> raise.error @@ Errors.generic_error Location.generated "empty record"
      | [(s,t)], v -> [s,(v,t)]
      | [(sa,ta);(sb,tb)], v when Option.is_some (Ligo_interpreter.Combinators.get_pair v) ->
          let (va, vb) = trace_option ~raise (Errors.generic_error Location.generated "Expected pair") @@
                           Ligo_interpreter.Combinators.get_pair v in
          let a' = aux [sa, ta] va in
          let b' = aux [sb, tb] vb in
          (a' @ b')
      | (shd,thd)::tl, v when Option.is_some (Ligo_interpreter.Combinators.get_pair v) ->
        let (va, vb) = trace_option ~raise (Errors.generic_error Location.generated "Expected pair") @@
                           Ligo_interpreter.Combinators.get_pair v in
        let tl' = aux tl vb in
        ((shd,(va,thd))::tl')
      | _ -> raise.error @@ Errors.generic_error Location.generated "bad record path"
    in
    aux lst v
  )

let extract_constructor ~raise ~(layout:Layout.t) (v : value) (lst : (Label.t * AST.type_expression) list) : (Label.t * value * AST.type_expression) =
  match layout with
  | L_tree ->
    let open Append_tree in
    let tree = match Append_tree.of_list lst with
      | Empty -> raise.error @@ Errors.generic_error Location.generated "empty variant"
      | Full t -> t in
    let rec aux tv : (Label.t * value * AST.type_expression) =
      match tv with
      | Leaf (k, t), v -> (k, v, t)
      | Node {a;b=_;size=_;full=_}, V_Construct ("Left", v) -> aux (a, v)
      | Node {a=_;b;size=_;full=_}, V_Construct ("Right", v) -> aux (b, v)
      | _ -> raise.error @@ Errors.generic_error Location.generated "bad constructor path"
    in
    let (s, v, t) = aux (tree, v) in
    (s, v, t)
  | L_comb -> (
    let rec aux tv : (Label.t * value * AST.type_expression) =
      match tv with
      | [], _ -> failwith "lal"
      | ((l,t)::tl), v-> ( match v with
        | V_Construct ("Left", v) -> (l,v,t)
        | V_Construct ("Right", v) -> aux (tl,v)
        | v -> (l,v,t)
      )
    in
    aux (lst,v)
  )
