open Trace
open Types
open Combinators
open Misc

let program_to_main : program -> string -> lambda result = fun p s ->
  let%bind (main , input_type , output_type) =
    let pred = fun d ->
      match d with
      | Declaration_constant (d , _) when d.name = s -> Some d.annotated_expression
      | Declaration_constant _ -> None
    in
    let%bind main =
      trace_option (simple_error "no main with given name") @@
      List.find_map (Function.compose pred Location.unwrap) p in
    let%bind (input_ty , output_ty) =
      match (get_type' @@ get_type_annotation main) with
      | T_function (i , o) -> ok (i , o)
      | _ -> simple_fail "program main isn't a function" in
    ok (main , input_ty , output_ty)
  in
  let body =
    let aux : declaration -> instruction = fun d ->
      match d with
      | Declaration_constant (d , _) -> I_declaration d in
    List.map (Function.compose aux Location.unwrap) p in
  let env =
    let aux = fun _ d ->
      match d with
      | Declaration_constant (_ , env) -> env in
    List.fold_left aux Environment.full_empty (List.map Location.unwrap p) in
  let binder = "@contract_input" in
  let result =
    let input_expr = e_a_variable binder input_type env in
    let main_expr = e_a_variable s (get_type_annotation main) env in
    e_a_application main_expr input_expr env in
  ok {
    binder ;
    input_type ;
    output_type ;
    body ;
    result ;
  }

module Captured_variables = struct

  type bindings = string list
  let mem : string -> bindings -> bool = List.mem
  let singleton : string -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : string list -> bindings = fun x -> x

  let rec annotated_expression : bindings -> annotated_expression -> bindings result = fun b ae ->
    let self = annotated_expression b in
    match ae.expression with
    | E_lambda l -> ok @@ Free_variables.lambda empty l
    | E_literal _ -> ok empty
    | E_constant (_ , lst) ->
      let%bind lst' = bind_map_list self lst in
      ok @@ unions lst'
    | E_variable name -> (
        let%bind env_element =
          trace_option (simple_error "missing var in env") @@
          Environment.get_opt name ae.environment in
        match env_element.definition with
        | ED_binder -> ok empty
        | ED_declaration (_ , _) -> simple_fail "todo"
      )
    | E_application (a, b) ->
      let%bind lst' = bind_map_list self [ a ; b ] in
      ok @@ unions lst'
    | E_tuple lst ->
      let%bind lst' = bind_map_list self lst in
      ok @@ unions lst'
    | E_constructor (_ , a) -> self a
    | E_record m ->
      let%bind lst' = bind_map_list self @@ Map.String.to_list m in
      ok @@ unions lst'
    | E_record_accessor (a, _) -> self a
    | E_tuple_accessor (a, _) -> self a
    | E_list lst ->
      let%bind lst' = bind_map_list self lst in
      ok @@ unions lst'
    | E_map m ->
      let%bind lst' = bind_map_list self @@ List.concat @@ List.map (fun (a, b) -> [ a ; b ]) m in
      ok @@ unions lst'
    | E_look_up (a , b) ->
      let%bind lst' = bind_map_list self [ a ; b ] in
      ok @@ unions lst'
    | E_matching (a , cs) ->
      let%bind a' = self a in
      let%bind cs' = matching_expression b cs in
      ok @@ union a' cs'
    | E_failwith a -> self a

  and instruction' : bindings -> instruction -> (bindings * bindings) result = fun b i ->
    match i with
    | I_declaration n ->
      let bounds = union (singleton n.name) b in
      let%bind frees = annotated_expression b n.annotated_expression in
      ok (bounds , frees)
    | I_assignment n ->
      let%bind frees = annotated_expression b n.annotated_expression in
      ok (b , frees)
    | I_skip -> ok (b , empty)
    | I_do e ->
      let%bind frees = annotated_expression b e in
      ok (b , frees)
    | I_loop (a , bl) ->
      let%bind ae_frees = annotated_expression b a in
      let%bind bl_frees = block b bl in
      ok (b , union ae_frees bl_frees)
    | I_patch (_ , _ , a) ->
      let%bind a' = annotated_expression b a in
      ok (b , a')
    | I_matching (a , cs) ->
      let%bind ae' = annotated_expression b a in
      let%bind bl' = matching_block b cs in
      ok (b , union ae' bl')

  and block' : bindings -> block -> (bindings * bindings) result = fun b bl ->
    let aux = fun (binds, frees) cur ->
      let%bind (binds', frees') = instruction' binds cur in
      ok (binds', union frees frees') in
    bind_fold_list aux (b , []) bl

  and block : bindings -> block -> bindings result = fun b bl ->
    let%bind (_ , frees) = block' b bl in
    ok frees

  and matching_variant_case : type a . (bindings -> a -> bindings result) -> bindings -> ((constructor_name * name) * a) -> bindings result  = fun f b ((_,n),c) ->
    f (union (singleton n) b) c

  and matching : type a . (bindings -> a -> bindings result) -> bindings -> a matching -> bindings result = fun f b m ->
    match m with
    | Match_bool { match_true = t ; match_false = fa } ->
      let%bind t' = f b t in
      let%bind fa' = f b fa in
      ok @@ union t' fa'
    | Match_list { match_nil = n ; match_cons = (hd, tl, c) } ->
      let%bind n' = f b n in
      let%bind c' = f (union (of_list [hd ; tl]) b) c in
      ok @@ union n' c'
    | Match_option { match_none = n ; match_some = ((opt, _), s) } ->
      let%bind n' = f b n in
      let%bind s' = f (union (singleton opt) b) s in
      ok @@ union n' s'
    | Match_tuple (lst , a) ->
      f (union (of_list lst) b) a
    | Match_variant (lst , _) ->
      let%bind lst' = bind_map_list (matching_variant_case f b) lst in
      ok @@ unions lst'

  and matching_expression = fun x -> matching annotated_expression x

  and matching_block = fun x -> matching block x

end
