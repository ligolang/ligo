(* The Transpiler is a function that takes as input the Typed AST, and outputs expressions in a language that is basically a Michelson with named variables and first-class-environments.

For more info, see back-end.md: https://gitlab.com/ligolang/ligo/blob/dev/gitlab-pages/docs/contributors/big-picture/back-end.md *)

open Trace
open Helpers

module AST = Ast_typed
module Append_tree = Tree.Append
open AST.Combinators
open Mini_c

let untranspile = Untranspiler.untranspile

let temp_unwrap_loc = Location.unwrap
let temp_unwrap_loc_list = List.map Location.unwrap

module Errors = struct
  let corner_case ~loc message =
    let title () = "corner case" in
    let content () = "we don't have a good error message for this case. we are
striving find ways to better report them and find the use-cases that generate
them. please report this to the developers." in
    let data = [
      ("location" , fun () -> loc) ;
      ("message" , fun () -> message) ;
    ] in
    error ~data title content

  let no_type_variable name =
    let title () = "type variables can't be transpiled" in
    let content () = Format.asprintf "%a" Var.pp name in
    error title content

  let row_loc l = ("location" , fun () -> Format.asprintf "%a" Location.pp l)

  let unsupported_pattern_matching kind location =
    let title () = "unsupported pattern-matching" in
    let content () = Format.asprintf "%s patterns aren't supported yet" kind in
    let data = [
        row_loc location ;
      ] in
    error ~data title content

  let unsupported_iterator location =
    let title () = "unsupported iterator" in
    let content () = "only lambda are supported as iterators" in
    let data = [
        row_loc location ;
      ] in
    error ~data title content

  let not_functional_main location =
    let title () = "not functional main" in
    let content () = "main should be a function" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp location) ;
    ] in
    error ~data title content

  let bad_big_map location =
    let title () = "bad arguments for main" in
    let content () = "only one big_map per program which must appear
      on the left hand side of a pair in the contract's storage" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp location) ;
    ] in
    error ~data title content

  let missing_entry_point name =
    let title () = "missing entry point" in
    let content () = "no entry point with the given name" in
    let data = [
      ("name" , fun () -> name) ;
    ] in
    error ~data title content

  let wrong_mini_c_value expected_type actual =
    let title () = "illed typed intermediary value" in
    let content () = "type of intermediary value doesn't match what was expected" in
    let data = [
      ("expected_type" , fun () -> expected_type) ;
      ("actual" , fun () -> Format.asprintf "%a" Mini_c.PP.value actual ) ;
    ] in
    error ~data title content

  let bad_untranspile bad_type value =
    let title () = "untranspiling bad value" in
    let content () = Format.asprintf "can not untranspile %s" bad_type in
    let data = [
      ("bad_type" , fun () -> bad_type) ;
      ("value" , fun () -> Format.asprintf "%a" Mini_c.PP.value value) ;
    ] in
    error ~data title content

  let unknown_untranspile unknown_type value =
    let title () = "untranspiling unknown value" in
    let content () = Format.asprintf "can not untranspile %s" unknown_type in
    let data = [
      ("unknown_type" , fun () -> unknown_type) ;
      ("value" , fun () -> Format.asprintf "%a" Mini_c.PP.value value) ;
    ] in
    error ~data title content

end
open Errors

let rec transpile_type (t:AST.type_expression) : type_value result =
  match t.type_content with
  | T_variable (name) -> fail @@ no_type_variable @@ name
  | T_constant (TC_bool) -> ok (T_base TC_bool)
  | T_constant (TC_int) -> ok (T_base TC_int)
  | T_constant (TC_nat) -> ok (T_base TC_nat)
  | T_constant (TC_mutez) -> ok (T_base TC_mutez)
  | T_constant (TC_string) -> ok (T_base TC_string)
  | T_constant (TC_bytes) -> ok (T_base TC_bytes)
  | T_constant (TC_address) -> ok (T_base TC_address)
  | T_constant (TC_timestamp) -> ok (T_base TC_timestamp)
  | T_constant (TC_unit) -> ok (T_base TC_unit)
  | T_constant (TC_operation) -> ok (T_base TC_operation)
  | T_constant (TC_signature) -> ok (T_base TC_signature)
  | T_constant (TC_key) -> ok (T_base TC_key)
  | T_constant (TC_key_hash) -> ok (T_base TC_key_hash)
  | T_constant (TC_chain_id) -> ok (T_base TC_chain_id)
  | T_constant (TC_void)     -> ok (T_base TC_void)
  | T_operator (TC_contract x) ->
      let%bind x' = transpile_type x in
      ok (T_contract x')
  | T_operator (TC_map (key,value)) ->
      let%bind kv' = bind_map_pair transpile_type (key, value) in
      ok (T_map kv')
  | T_operator (TC_big_map (key,value)) ->
      let%bind kv' = bind_map_pair transpile_type (key, value) in
      ok (T_big_map kv')
  | T_operator (TC_list t) ->
      let%bind t' = transpile_type t in
      ok (T_list t')
  | T_operator (TC_set t) ->
      let%bind t' = transpile_type t in
      ok (T_set t')
  | T_operator (TC_option o) ->
      let%bind o' = transpile_type o in
      ok (T_option o')
  | T_operator (TC_arrow (param , result)) -> (
      let%bind param' = transpile_type param in
      let%bind result' = transpile_type result in
      ok (T_function (param', result'))
    )
  (* TODO hmm *)
  | T_sum m ->
      let node = Append_tree.of_list @@ kv_list_of_cmap m in
      let aux a b : type_value annotated result =
        let%bind a = a in
        let%bind b = b in
        ok (None, T_or (a, b))
      in
      let%bind m' = Append_tree.fold_ne
                      (fun (Stage_common.Types.Constructor ann, a) ->
                        let%bind a = transpile_type a in
                        ok (Some (String.uncapitalize_ascii ann), a))
                      aux node in
      ok @@ snd m'
  | T_record m ->
      let node = Append_tree.of_list @@ kv_list_of_lmap m in
      let aux a b : type_value annotated result =
        let%bind a = a in
        let%bind b = b in
        ok (None, T_pair (a, b))
      in
      let%bind m' = Append_tree.fold_ne
                      (fun (Stage_common.Types.Label ann, a) ->
                        let%bind a = transpile_type a in
                        ok (Some ann, a))
                      aux node in
      ok @@ snd m'
  | T_arrow {type1;type2} -> (
      let%bind param' = transpile_type type1 in
      let%bind result' = transpile_type type2 in
      ok (T_function (param',result'))
    )

let record_access_to_lr : type_value -> type_value AST.label_map -> AST.label -> (type_value * [`Left | `Right]) list result = fun ty tym ind ->
  let tys = kv_list_of_lmap tym in
  let node_tv = Append_tree.of_list tys in
  let%bind path =
    let aux (i , _) = i = ind  in
    trace_option (corner_case ~loc:__LOC__ "record access leaf") @@
    Append_tree.exists_path aux node_tv in
  let lr_path = List.map (fun b -> if b then `Right else `Left) path in
  let%bind (_ , lst) =
    let aux = fun (ty , acc) cur ->
      let%bind (a , b) =
        trace_strong (corner_case ~loc:__LOC__ "record access pair") @@
        Mini_c.get_t_pair ty in
      match cur with
      | `Left -> ok (a , acc @ [(a , `Left)])
      | `Right -> ok (b , acc @ [(b , `Right)] ) in
    bind_fold_list aux (ty , []) lr_path in
  ok lst

let rec transpile_literal : AST.literal -> value = fun l -> match l with
  | Literal_bool b -> D_bool b
  | Literal_int n -> D_int n
  | Literal_nat n -> D_nat n
  | Literal_timestamp n -> D_timestamp n
  | Literal_mutez n -> D_mutez n
  | Literal_bytes s -> D_bytes s
  | Literal_string s -> D_string s
  | Literal_address s -> D_string s
  | Literal_signature s -> D_string s
  | Literal_key s -> D_string s
  | Literal_key_hash s -> D_string s
  | Literal_chain_id s -> D_string s
  | Literal_operation op -> D_operation op
  | Literal_unit -> D_unit
  | Literal_void -> D_none

and transpile_environment_element_type : AST.environment_element -> type_value result = fun ele ->
  transpile_type ele.type_value

and tree_of_sum : AST.type_expression -> (AST.constructor' * AST.type_expression) Append_tree.t result = fun t ->
  let%bind map_tv = get_t_sum t in
  ok @@ Append_tree.of_list @@ kv_list_of_cmap map_tv

and transpile_annotated_expression (ae:AST.expression) : expression result =
  let%bind tv = transpile_type ae.type_expression in
  let return ?(tv = tv) expr = ok @@ Combinators.Expression.make_tpl (expr, tv) in
  let f = transpile_annotated_expression in
  let info =
    let title () = "translating expression" in
    let content () = Format.asprintf "%a" Location.pp ae.location in
    info title content in
  trace info @@
  match ae.expression_content with
  | E_let_in {let_binder; rhs; let_result; inline} ->
    let%bind rhs' = transpile_annotated_expression rhs in
    let%bind result' = transpile_annotated_expression let_result in
    return (E_let_in ((let_binder, rhs'.type_value), inline, rhs', result'))
  | E_literal l -> return @@ E_literal (transpile_literal l)
  | E_variable name -> (
      let%bind ele =
        trace_option (corner_case ~loc:__LOC__ "name not in environment") @@
        AST.Environment.get_opt name ae.environment in
      let%bind tv = transpile_environment_element_type ele in
      return ~tv @@ E_variable (name)
    )
  | E_application {expr1;expr2} ->
      let%bind a = transpile_annotated_expression expr1 in
      let%bind b = transpile_annotated_expression expr2 in
      return @@ E_application (a, b)
  | E_constructor {constructor;element} -> (
      let%bind param' = transpile_annotated_expression element in
      let (param'_expr , param'_tv) = Combinators.Expression.(get_content param' , get_type param') in
      let%bind node_tv =
        trace_strong (corner_case ~loc:__LOC__ "getting lr tree") @@
        tree_of_sum ae.type_expression in
      let leaf (k, tv) : (expression' option * type_value) result =
        if k = constructor then (
          let%bind _ =
            trace_strong (corner_case ~loc:__LOC__ "wrong type for constructor parameter")
            @@ AST.assert_type_expression_eq (tv, element.type_expression) in
          ok (Some (param'_expr), param'_tv)
        ) else (
          let%bind tv = transpile_type tv in
          ok (None, tv)
        ) in
      let node a b : (expression' option * type_value) result =
        let%bind a = a in
        let%bind b = b in
        match (a, b) with
        | (None, a), (None, b) -> ok (None, T_or ((None, a), (None, b)))
        | (Some _, _), (Some _, _) -> fail @@ corner_case ~loc:__LOC__ "multiple identical constructors in the same variant"
        | (Some v, a), (None, b) -> ok (Some (E_constant {cons_name=C_LEFT ;arguments= [Combinators.Expression.make_tpl (v, a)]}), T_or ((None, a), (None, b)))
        | (None, a), (Some v, b) -> ok (Some (E_constant {cons_name=C_RIGHT;arguments= [Combinators.Expression.make_tpl (v, b)]}), T_or ((None, a), (None, b)))
      in
      let%bind (ae_opt, tv) = Append_tree.fold_ne leaf node node_tv in
      let%bind ae =
        trace_option (corner_case ~loc:__LOC__ "inexistant constructor")
          ae_opt in
      return ~tv ae
    )
  | E_record m -> (
      let node = Append_tree.of_list @@ list_of_lmap m in
      let aux a b : expression result =
        let%bind a = a in
        let%bind b = b in
        let a_ty = Combinators.Expression.get_type a in
        let b_ty = Combinators.Expression.get_type b in
        let tv = T_pair ((None, a_ty) , (None, b_ty)) in
        return ~tv @@ E_constant {cons_name=C_PAIR;arguments=[a; b]}
      in
      trace_strong (corner_case ~loc:__LOC__ "record build") @@
      Append_tree.fold_ne (transpile_annotated_expression) aux node
    )
  | E_record_accessor {expr; label} ->
      let%bind ty' = transpile_type (get_type_expression expr) in
      let%bind ty_lmap =
        trace_strong (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record (get_type_expression expr) in
      let%bind ty'_lmap = Stage_common.Helpers.bind_map_lmap transpile_type ty_lmap in
      let%bind path =
        trace_strong (corner_case ~loc:__LOC__ "record access") @@
        record_access_to_lr ty' ty'_lmap label in
      let aux = fun pred (ty, lr) ->
        let c = match lr with
          | `Left  -> C_CAR
          | `Right -> C_CDR in
        Combinators.Expression.make_tpl (E_constant {cons_name=c;arguments=[pred]} , ty) in
      let%bind record' = transpile_annotated_expression expr in
      let expr = List.fold_left aux record' path in
      ok expr
  | E_record_update {record; path; update} -> 
      let%bind ty' = transpile_type (get_type_expression record) in 
      let%bind ty_lmap =
        trace_strong (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record (get_type_expression record) in
      let%bind ty'_lmap = Stage_common.Helpers.bind_map_lmap transpile_type ty_lmap in
      let%bind path = 
        trace_strong (corner_case ~loc:__LOC__ "record access") @@
        record_access_to_lr ty' ty'_lmap path in
      let path = List.map snd path in
      let%bind update = transpile_annotated_expression update in
      let%bind record = transpile_annotated_expression record in
      return @@ E_record_update (record, path, update)
  | E_constant {cons_name=name; arguments=lst} -> (
      let iterator_generator iterator_name =
        let lambda_to_iterator_body (f : AST.expression) (l : AST.lambda) =
          let%bind body' = transpile_annotated_expression l.result in
          let%bind (input , _) = AST.get_t_function f.type_expression in
          let%bind input' = transpile_type input in
          ok ((l.binder , input') , body')
        in
        let expression_to_iterator_body (f : AST.expression) =
          match f.expression_content with
          | E_lambda l -> lambda_to_iterator_body f l
          | E_variable v -> (
              let%bind elt =
                trace_option (corner_case ~loc:__LOC__ "missing var") @@
                AST.Environment.get_opt v f.environment in
              match elt.definition with
              | ED_declaration (f , _) -> (
                  match f.expression_content with
                  | E_lambda l -> lambda_to_iterator_body f l
                  | _ -> fail @@ unsupported_iterator f.location
                )
              | _ -> fail @@ unsupported_iterator f.location
            )
          | _ -> fail @@ unsupported_iterator f.location
        in
        fun (lst : AST.expression list) -> match (lst , iterator_name) with
          | [f ; i] , C_ITER | [f ; i] , C_MAP -> (
              let%bind f' = expression_to_iterator_body f in
              let%bind i' = transpile_annotated_expression i in
              return @@ E_iterator (iterator_name , f' , i')
            )
          | [ f ; collection ; initial ] , C_FOLD -> (
              let%bind f' = expression_to_iterator_body f in
              let%bind initial' = transpile_annotated_expression initial in
              let%bind collection' = transpile_annotated_expression collection in
              return @@ E_fold (f' , collection' , initial')
            )
          | _ -> fail @@ corner_case ~loc:__LOC__ (Format.asprintf "bad iterator arity: %a" Stage_common.PP.constant iterator_name)
      in
      let (iter , map , fold) = iterator_generator C_ITER, iterator_generator C_MAP, iterator_generator C_FOLD in
      match (name , lst) with
      | (C_SET_ITER , lst) -> iter lst
      | (C_LIST_ITER , lst) -> iter lst
      | (C_MAP_ITER , lst) -> iter lst
      | (C_LIST_MAP , lst) -> map lst
      | (C_MAP_MAP , lst) -> map lst
      | (C_LIST_FOLD , lst) -> fold lst
      | (C_SET_FOLD , lst) -> fold lst
      | (C_MAP_FOLD , lst) -> fold lst
      | _ -> (
          let%bind lst' = bind_map_list (transpile_annotated_expression) lst in
          return @@ E_constant {cons_name=name;arguments=lst'}
        )
    )
  | E_lambda l ->
    let%bind io = AST.get_t_function ae.type_expression in
    transpile_lambda l io
  | E_list lst -> (
      let%bind t =
        trace_strong (corner_case ~loc:__LOC__ "not a list") @@
        get_t_list tv in
      let%bind lst' = bind_map_list (transpile_annotated_expression) lst in
      let aux : expression -> expression -> expression result = fun prev cur ->
        return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
      let%bind (init : expression) = return @@ E_make_empty_list t in
      bind_fold_right_list aux init lst'
    )
  | E_set lst -> (
      let%bind t =
        trace_strong (corner_case ~loc:__LOC__ "not a set") @@
        get_t_set tv in
      let%bind lst' = bind_map_list (transpile_annotated_expression) lst in
      let aux : expression -> expression -> expression result = fun prev cur ->
        return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
      let%bind (init : expression) = return @@ E_make_empty_set t in
      bind_fold_list aux init lst'
    )
  | E_map m -> (
      let%bind (src, dst) =
        trace_strong (corner_case ~loc:__LOC__ "not a map") @@
        Mini_c.Combinators.get_t_map tv in
      let aux : expression result -> (AST.expression * AST.expression) -> expression result = fun prev (k, v) ->
        let%bind prev' = prev in
        let%bind (k', v') =
          let v' = e_a_some v ae.environment in
          bind_map_pair (transpile_annotated_expression) (k , v') in
        return @@ E_constant {cons_name=C_UPDATE;arguments=[k' ; v' ; prev']}
      in
      let init = return @@ E_make_empty_map (src, dst) in
      List.fold_left aux init m
    )
  | E_big_map m -> (
      let%bind (src, dst) =
        trace_strong (corner_case ~loc:__LOC__ "not a map") @@
        Mini_c.Combinators.get_t_big_map tv in
      let aux : expression result -> (AST.expression * AST.expression) -> expression result = fun prev (k, v) ->
        let%bind prev' = prev in
        let%bind (k', v') =
          let v' = e_a_some v ae.environment in
          bind_map_pair (transpile_annotated_expression) (k , v') in
        return @@ E_constant {cons_name=C_UPDATE;arguments=[k' ; v' ; prev']}
      in
      let init = return @@ E_make_empty_big_map (src, dst) in
      List.fold_left aux init m
    )
  | E_look_up dsi -> (
      let%bind (ds', i') = bind_map_pair f dsi in
      return @@ E_constant {cons_name=C_MAP_FIND_OPT;arguments=[i' ; ds']}
    )
  | E_loop {condition; body} -> (
      let%bind expr' = transpile_annotated_expression condition in
      let%bind body' = transpile_annotated_expression body in
      return @@ E_while (expr' , body')
    )
  | E_matching {matchee=expr; cases=m} -> (
      let%bind expr' = transpile_annotated_expression expr in
      match m with
      | Match_bool {match_true ; match_false} ->
          let%bind (t , f) = bind_map_pair (transpile_annotated_expression) (match_true, match_false) in
          return @@ E_if_bool (expr', t, f)
      | Match_option { match_none; match_some = (name, s, tv) } ->
          let%bind n = transpile_annotated_expression match_none in
          let%bind (tv' , s') =
            let%bind tv' = transpile_type tv in
            let%bind s' = transpile_annotated_expression s in
            ok (tv' , s')
          in
          return @@ E_if_none (expr' , n , ((name , tv') , s'))
      | Match_list {
          match_nil ;
          match_cons = ((hd_name) , (tl_name), match_cons, ty) ;
        } -> (
          let%bind nil = transpile_annotated_expression match_nil in
          let%bind cons =
            let%bind ty' = transpile_type ty in
            let%bind match_cons' = transpile_annotated_expression match_cons in
            ok (((hd_name , ty') , (tl_name , ty')) , match_cons')
          in
          return @@ E_if_cons (expr' , nil , cons)
        )
      | Match_variant (lst , variant) -> (
          let%bind tree =
            trace_strong (corner_case ~loc:__LOC__ "getting lr tree") @@
            tree_of_sum variant in
          let%bind tree' = match tree with
            | Empty -> fail (corner_case ~loc:__LOC__ "match empty variant")
            | Full x -> ok x in
          let%bind tree'' =
            let rec aux t =
              match (t : _ Append_tree.t') with
              | Leaf (name , tv) ->
                  let%bind tv' = transpile_type tv in
                  ok (`Leaf name , tv')
              | Node {a ; b} ->
                  let%bind a' = aux a in
                  let%bind b' = aux b in
                  let tv' = Mini_c.t_union (None, snd a') (None, snd b') in
                  ok (`Node (a' , b') , tv')
            in aux tree'
          in

          let rec aux top t =
            match t with
            | ((`Leaf constructor_name) , tv) -> (
                let%bind ((_ , name) , body) =
                  trace_option (corner_case ~loc:__LOC__ "missing match clause") @@
                  List.find_opt (fun ((constructor_name' , _) , _) -> constructor_name' = constructor_name) lst in
                let%bind body' = transpile_annotated_expression body in
                return @@ E_let_in ((name , tv) , false , top , body')
              )
            | ((`Node (a , b)) , tv) ->
                let%bind a' =
                  let%bind a_ty = get_t_left tv in
                  let left_var = Var.fresh ~name:"left" () in
                  let%bind e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                  ok ((left_var , a_ty) , e)
                in
                let%bind b' =
                  let%bind b_ty = get_t_right tv in
                  let right_var = Var.fresh ~name:"right" () in
                  let%bind e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                  ok ((right_var , b_ty) , e)
                in
                return @@ E_if_left (top , a' , b')
          in
          trace_strong (corner_case ~loc:__LOC__ "building constructor") @@
          aux expr' tree''
       )
      | AST.Match_tuple _ -> fail @@ unsupported_pattern_matching "tuple" ae.location
  )

and transpile_lambda l (input_type , output_type) =
  let { binder ; result } : AST.lambda = l in
  let%bind result' = transpile_annotated_expression result in
  let%bind input = transpile_type input_type in
  let%bind output = transpile_type output_type in
  let tv = Combinators.t_function input output in
  let binder = binder in 
  let closure = E_closure { binder; body = result'} in
  ok @@ Combinators.Expression.make_tpl (closure , tv)

let transpile_declaration env (d:AST.declaration) : toplevel_statement result =
  match d with
  | Declaration_constant (name,expression, inline, _) ->
      let name = name in
      let%bind expression = transpile_annotated_expression expression in
      let tv = Combinators.Expression.get_type expression in
      let env' = Environment.add (name, tv) env in
      ok @@ ((name, inline, expression), environment_wrap env env')

let transpile_program (lst : AST.program) : program result =
  let aux (prev:(toplevel_statement list * Environment.t) result) cur =
    let%bind (hds, env) = prev in
    let%bind ((_, env') as cur') = transpile_declaration env cur in
    ok (hds @ [ cur' ], env'.post_environment)
  in
  let%bind (statements, _) = List.fold_left aux (ok ([], Environment.empty)) (temp_unwrap_loc_list lst) in
  ok statements

(* check whether the storage contains a big_map, if yes, check that
  it appears on the left hand side of a pair *)
let check_storage f ty loc : (anon_function * _) result =
  let rec aux (t:type_value) on_big_map =
    match t with
      | T_big_map _ -> on_big_map
      | T_pair (a , b) -> (aux (snd a) true) && (aux (snd b) false)
      | T_or (a,b) -> (aux (snd a) false) && (aux (snd b) false)
      | T_function (a,b) -> (aux a false) && (aux b false)
      | T_map (a,b) -> (aux a false) && (aux b false)
      | T_list a -> (aux a false)
      | T_set a -> (aux a false)
      | T_contract a -> (aux a false)
      | T_option a -> (aux a false)
      | _ -> true
  in
  match f.body.type_value with
    | T_pair (_, storage) ->
      if aux (snd storage) false then ok (f, ty) else fail @@ bad_big_map loc
    | _ -> ok (f, ty)

let extract_constructor (v : value) (tree : _ Append_tree.t') : (string * value * AST.type_expression) result =
  let open Append_tree in
  let rec aux tv : (string * value * AST.type_expression) result=
    match tv with
    | Leaf (k, t), v -> ok (k, v, t)
    | Node {a}, D_left v -> aux (a, v)
    | Node {b}, D_right v -> aux (b, v)
    | _ -> fail @@ internal_assertion_failure "bad constructor path"
  in
  let%bind (s, v, t) = aux (tree, v) in
  ok (s, v, t)

let extract_tuple (v : value) (tree : AST.type_expression Append_tree.t') : ((value * AST.type_expression) list) result =
  let open Append_tree in
  let rec aux tv : ((value * AST.type_expression) list) result =
    match tv with
    | Leaf t, v -> ok @@ [v, t]
    | Node {a;b}, D_pair (va, vb) ->
        let%bind a' = aux (a, va) in
        let%bind b' = aux (b, vb) in
        ok (a' @ b')
    | _ -> fail @@ internal_assertion_failure "bad tuple path"
  in
  aux (tree, v)

let extract_record (v : value) (tree : _ Append_tree.t') : (_ list) result =
  let open Append_tree in
  let rec aux tv : ((string * (value * AST.type_expression)) list) result =
    match tv with
    | Leaf (s, t), v -> ok @@ [s, (v, t)]
    | Node {a;b}, D_pair (va, vb) ->
        let%bind a' = aux (a, va) in
        let%bind b' = aux (b, vb) in
        ok (a' @ b')
    | _ -> fail @@ internal_assertion_failure "bad record path"
  in
  aux (tree, v)
