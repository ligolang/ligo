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
  
  let unsupported_recursive_function expression_variable =
    let title () = "unsupported recursive function yet" in
    let content () = "only fuction with one variable are supported" in
    let data = [
      ("value" , fun () -> Format.asprintf "%a" AST.PP.expression_variable expression_variable) ;
    ] in
    error ~data title content


end
open Errors

let transpile_constant' : AST.constant' -> constant' = function
  | C_INT -> C_INT
  | C_UNIT -> C_UNIT
  | C_NIL -> C_NIL
  | C_NOW -> C_NOW
  | C_IS_NAT -> C_IS_NAT
  | C_SOME -> C_SOME
  | C_NONE -> C_NONE
  | C_ASSERTION -> C_ASSERTION
  | C_ASSERT_INFERRED -> C_ASSERT_INFERRED
  | C_FAILWITH -> C_FAILWITH
  | C_UPDATE -> C_UPDATE
  (* Loops *)
  | C_ITER -> C_ITER
  | C_FOLD_WHILE -> C_FOLD_WHILE
  | C_FOLD_CONTINUE -> C_FOLD_CONTINUE
  | C_FOLD_STOP -> C_FOLD_STOP
  | C_LOOP_LEFT -> C_LOOP_LEFT
  | C_LOOP_CONTINUE -> C_LOOP_CONTINUE
  | C_LOOP_STOP -> C_LOOP_STOP
  | C_FOLD -> C_FOLD
  (* MATH *)
  | C_NEG -> C_NEG
  | C_ABS -> C_ABS
  | C_ADD -> C_ADD
  | C_SUB -> C_SUB
  | C_MUL -> C_MUL
  | C_EDIV -> C_EDIV
  | C_DIV -> C_DIV
  | C_MOD -> C_MOD
  (* LOGIC *)
  | C_NOT -> C_NOT
  | C_AND -> C_AND
  | C_OR -> C_OR
  | C_XOR -> C_XOR
  | C_LSL -> C_LSL
  | C_LSR -> C_LSR
  (* COMPARATOR *)
  | C_EQ -> C_EQ
  | C_NEQ -> C_NEQ
  | C_LT -> C_LT
  | C_GT -> C_GT
  | C_LE -> C_LE
  | C_GE -> C_GE
  (* Bytes/ String *)
  | C_SIZE -> C_SIZE
  | C_CONCAT -> C_CONCAT
  | C_SLICE -> C_SLICE
  | C_BYTES_PACK -> C_BYTES_PACK
  | C_BYTES_UNPACK -> C_BYTES_UNPACK
  | C_CONS -> C_CONS
  (* Pair *)
  | C_PAIR -> C_PAIR
  | C_CAR -> C_CAR
  | C_CDR -> C_CDR
  | C_LEFT -> C_LEFT
  | C_RIGHT -> C_RIGHT
  (* Set *)
  | C_SET_EMPTY -> C_SET_EMPTY
  | C_SET_LITERAL -> C_SET_LITERAL
  | C_SET_ADD -> C_SET_ADD
  | C_SET_REMOVE -> C_SET_REMOVE
  | C_SET_ITER -> C_SET_ITER
  | C_SET_FOLD -> C_SET_FOLD
  | C_SET_MEM -> C_SET_MEM
  (* List *)
  | C_LIST_EMPTY -> C_LIST_EMPTY
  | C_LIST_LITERAL -> C_LIST_LITERAL
  | C_LIST_ITER -> C_LIST_ITER
  | C_LIST_MAP -> C_LIST_MAP
  | C_LIST_FOLD -> C_LIST_FOLD
  (* Maps *)
  | C_MAP -> C_MAP
  | C_MAP_EMPTY -> C_MAP_EMPTY
  | C_MAP_LITERAL -> C_MAP_LITERAL
  | C_MAP_GET -> C_MAP_GET
  | C_MAP_GET_FORCE -> C_MAP_GET_FORCE
  | C_MAP_ADD -> C_MAP_ADD
  | C_MAP_REMOVE -> C_MAP_REMOVE
  | C_MAP_UPDATE -> C_MAP_UPDATE
  | C_MAP_ITER -> C_MAP_ITER
  | C_MAP_MAP -> C_MAP_MAP
  | C_MAP_FOLD -> C_MAP_FOLD
  | C_MAP_MEM -> C_MAP_MEM
  | C_MAP_FIND -> C_MAP_FIND
  | C_MAP_FIND_OPT -> C_MAP_FIND_OPT
  (* Big Maps *)
  | C_BIG_MAP -> C_BIG_MAP
  | C_BIG_MAP_EMPTY -> C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL -> C_BIG_MAP_LITERAL
  (* Crypto *)
  | C_SHA256 -> C_SHA256
  | C_SHA512 -> C_SHA512
  | C_BLAKE2b -> C_BLAKE2b
  | C_HASH -> C_HASH
  | C_HASH_KEY -> C_HASH_KEY
  | C_CHECK_SIGNATURE -> C_CHECK_SIGNATURE
  | C_CHAIN_ID -> C_CHAIN_ID
  (* Blockchain *)
  | C_CALL -> C_CALL
  | C_CONTRACT -> C_CONTRACT
  | C_CONTRACT_OPT -> C_CONTRACT_OPT
  | C_CONTRACT_ENTRYPOINT -> C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT -> C_CONTRACT_ENTRYPOINT_OPT
  | C_AMOUNT -> C_AMOUNT
  | C_BALANCE -> C_BALANCE
  | C_SOURCE -> C_SOURCE
  | C_SENDER -> C_SENDER
  | C_ADDRESS -> C_ADDRESS
  | C_SELF -> C_SELF
  | C_SELF_ADDRESS -> C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT -> C_IMPLICIT_ACCOUNT
  | C_SET_DELEGATE -> C_SET_DELEGATE
  | C_CREATE_CONTRACT -> C_CREATE_CONTRACT

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
  | T_operator (TC_map {k;v}) ->
      let%bind kv' = bind_map_pair transpile_type (k, v) in
      ok (T_map kv')
  | T_operator (TC_big_map {k;v}) ->
      let%bind kv' = bind_map_pair transpile_type (k, v) in
      ok (T_big_map kv')
  | T_operator (TC_map_or_big_map _) ->
      fail @@ corner_case ~loc:"transpiler" "TC_map_or_big_map should have been resolved before transpilation"
  | T_operator (TC_list t) ->
      let%bind t' = transpile_type t in
      ok (T_list t')
  | T_operator (TC_set t) ->
      let%bind t' = transpile_type t in
      ok (T_set t')
  | T_operator (TC_option o) ->
      let%bind o' = transpile_type o in
      ok (T_option o')
  | T_operator (TC_arrow {type1=param ; type2=result}) -> (
      let%bind param' = transpile_type param in
      let%bind result' = transpile_type result in
      ok (T_function (param', result'))
    )
  | T_sum m when Ast_typed.Helpers.is_michelson_or m ->
      let node = Append_tree.of_list @@ kv_list_of_cmap m in
      let aux a b : type_value annotated result =
        let%bind a = a in
        let%bind b = b in
        ok (None, T_or (a, b))
      in
      let%bind m' = Append_tree.fold_ne
                      (fun (_, ({ctor_type ; michelson_annotation}: AST.ctor_content)) ->
                        let%bind a = transpile_type ctor_type in
                        ok (Ast_typed.Helpers.remove_empty_annotation michelson_annotation, a) )
                      aux node in
      ok @@ snd m'
  | T_sum m ->
      let node = Append_tree.of_list @@ kv_list_of_cmap m in
      let aux a b : type_value annotated result =
        let%bind a = a in
        let%bind b = b in
        ok (None, T_or (a, b))
      in
      let%bind m' = Append_tree.fold_ne
                      (fun (Ast_typed.Types.Constructor ann, ({ctor_type ; _}: AST.ctor_content)) ->
                        let%bind a = transpile_type ctor_type in
                        ok (Some (String.uncapitalize_ascii ann), a))
                      aux node in
      ok @@ snd m'
  | T_record m when Ast_typed.Helpers.is_michelson_pair m ->
      let node = Append_tree.of_list @@ Ast_typed.Helpers.tuple_of_record m in
      let aux a b : type_value annotated result =
        let%bind a = a in
        let%bind b = b in
        ok (None, T_pair (a, b))
      in
      let%bind m' = Append_tree.fold_ne
                      (fun (_, ({field_type ; michelson_annotation} : AST.field_content)) ->
                        let%bind a = transpile_type field_type in
                        ok (Ast_typed.Helpers.remove_empty_annotation michelson_annotation, a) )
                      aux node in
      ok @@ snd m'
  | T_record m ->
      let is_tuple_lmap = Ast_typed.Helpers.is_tuple_lmap m in
      let node = Append_tree.of_list @@ (
        if is_tuple_lmap then
          Ast_typed.Helpers.tuple_of_record m
        else 
          List.rev @@ Ast_typed.Types.LMap.to_kv_list m
        )
      in
      let aux a b : type_value annotated result =
        let%bind a = a in
        let%bind b = b in
        ok (None, T_pair (a, b))
      in
      let%bind m' = Append_tree.fold_ne
                      (fun (Ast_typed.Types.Label ann, ({field_type;_}: AST.field_content)) ->
                        let%bind a = transpile_type field_type in
                        ok ((if is_tuple_lmap then 
                              None 
                            else 
                              Some ann), 
                            a)
                      )
                      aux node in
      ok @@ snd m'
  | T_arrow {type1;type2} -> (
      let%bind param' = transpile_type type1 in
      let%bind result' = transpile_type type2 in
      ok (T_function (param',result'))
    )

let record_access_to_lr : type_value -> type_value AST.label_map -> AST.label -> (type_value * [`Left | `Right]) list result = fun ty tym ind ->
  let tys = Ast_typed.Helpers.kv_list_of_record_or_tuple tym in
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
  let kt_list = List.map (fun (k,({ctor_type;_}:AST.ctor_content)) -> (k,ctor_type)) (kv_list_of_cmap map_tv) in
  ok @@ Append_tree.of_list kt_list

and transpile_annotated_expression (ae:AST.expression) : expression result =
  let%bind tv = transpile_type ae.type_expression in
  let return ?(tv = tv) expr = ok @@ Combinators.Expression.make_tpl (expr, tv) in
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
  | E_application {lamb; args} ->
      let%bind a = transpile_annotated_expression lamb in
      let%bind b = transpile_annotated_expression args in
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
    (*list_of_lmap to record_to_list*)
      let node = Append_tree.of_list @@ Ast_typed.Helpers.list_of_record_or_tuple m in
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
  | E_record_accessor {record; path} ->
      let%bind ty' = transpile_type (get_type_expression record) in
      let%bind ty_lmap =
        trace_strong (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record (get_type_expression record) in
      let%bind ty'_lmap = Ast_typed.Helpers.bind_map_lmap_t transpile_type ty_lmap in
      let%bind path =
        trace_strong (corner_case ~loc:__LOC__ "record access") @@
        record_access_to_lr ty' ty'_lmap path in
      let aux = fun pred (ty, lr) ->
        let c = match lr with
          | `Left  -> C_CAR
          | `Right -> C_CDR in
        Combinators.Expression.make_tpl (E_constant {cons_name=c;arguments=[pred]} , ty) in
      let%bind record' = transpile_annotated_expression record in
      let expr = List.fold_left aux record' path in
      ok expr
  | E_record_update {record; path; update} -> 
      let rec aux res (r,p,up) =
        let ty = get_type_expression r in
        let%bind ty_lmap =
          trace_strong (corner_case ~loc:__LOC__ "not a record") @@
          get_t_record (ty) in
        let%bind ty' = transpile_type (ty) in 
        let%bind ty'_lmap = Ast_typed.Helpers.bind_map_lmap_t transpile_type ty_lmap in
        let%bind p' = 
          trace_strong (corner_case ~loc:__LOC__ "record access") @@
          record_access_to_lr ty' ty'_lmap p in
        let res' = res @ p' in
        match (up:AST.expression).expression_content with
        | AST.E_record_update {record=record'; path=path'; update=update'} -> (
          match record'.expression_content with 
            | AST.E_record_accessor {record;path} ->
              if (AST.Misc.equal_variables record r && path = p) then
                aux res' (record',path',update')
              else ok @@ (up,res')
            | _ -> ok @@ (up,res')
        )
        | _ -> ok @@ (up,res')
      in
      let%bind (update, path) = aux [] (record, path, update) in
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
              | ED_declaration { expr = f ; free_variables = _ } -> (
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
          return @@ E_constant {cons_name=transpile_constant' name;arguments=lst'}
        )
    )
  | E_lambda l ->
    let%bind io = AST.get_t_function ae.type_expression in
    transpile_lambda l io
  | E_recursive r ->
    transpile_recursive r
  | E_matching {matchee=expr; cases=m} -> (
      let%bind expr' = transpile_annotated_expression expr in
      match m with
      | Match_bool {match_true ; match_false} ->
          let%bind (t , f) = bind_map_pair (transpile_annotated_expression) (match_true, match_false) in
          return @@ E_if_bool (expr', t, f)
      | Match_option { match_none; match_some = {opt; body; tv} } ->
          let%bind n = transpile_annotated_expression match_none in
          let%bind (tv' , s') =
            let%bind tv' = transpile_type tv in
            let%bind s' = transpile_annotated_expression body in
            ok (tv' , s')
          in
          return @@ E_if_none (expr' , n , ((opt , tv') , s'))
      | Match_list {
          match_nil ;
          match_cons = {hd; tl; body; tv} ;
        } -> (
          let%bind nil = transpile_annotated_expression match_nil in
          let%bind cons =
            let%bind ty' = transpile_type tv in
            let%bind match_cons' = transpile_annotated_expression body in
            ok (((hd , ty') , (tl , ty')) , match_cons')
          in
          return @@ E_if_cons (expr' , nil , cons)
        )
      | Match_variant {cases ; tv} -> (
          let%bind tree =
            trace_strong (corner_case ~loc:__LOC__ "getting lr tree") @@
            tree_of_sum tv in
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
            | ((`Leaf (AST.Constructor constructor_name)) , tv) -> (
                let%bind {constructor=_ ; pattern ; body} =
                  trace_option (corner_case ~loc:__LOC__ "missing match clause") @@
                    let aux ({constructor = Constructor c ; pattern=_ ; body=_} : AST.matching_content_case) =
                      (c = constructor_name) in
                  List.find_opt aux cases in
                let%bind body' = transpile_annotated_expression body in
                return @@ E_let_in ((pattern , tv) , false , top , body')
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

and transpile_recursive {fun_name; fun_type; lambda} =
  let rec map_lambda : AST.expression_variable -> type_value -> AST.expression -> (expression * expression_variable list) result = fun fun_name loop_type e ->
    match e.expression_content with 
      E_lambda {binder;result} -> 
        let%bind (body,l) = map_lambda fun_name loop_type result in
        ok @@ (Expression.make (E_closure {binder;body}) loop_type, binder::l)
      | _  -> 
        let%bind res = replace_callback fun_name loop_type false e in
        ok @@ (res, [])

  and replace_callback : AST.expression_variable -> type_value -> bool -> AST.expression -> expression result = fun fun_name loop_type shadowed e ->
    match e.expression_content with
      E_let_in li -> 
        let shadowed = shadowed || Var.equal li.let_binder fun_name in 
        let%bind let_result = replace_callback fun_name loop_type shadowed li.let_result in
        let%bind rhs = transpile_annotated_expression li.rhs in
        let%bind ty  = transpile_type e.type_expression in
        ok @@ e_let_in li.let_binder ty li.inline rhs let_result |
      E_matching m -> 
        let%bind ty = transpile_type e.type_expression in
        matching fun_name loop_type shadowed m ty |
      E_application {lamb;args} -> (
        match lamb.expression_content,shadowed with
        E_variable name, false when Var.equal fun_name name -> 
          let%bind expr = transpile_annotated_expression args in
          ok @@ Expression.make (E_constant {cons_name=C_LOOP_CONTINUE;arguments=[expr]}) loop_type |
        _ -> 
          let%bind expr = transpile_annotated_expression e in
          ok @@ Expression.make (E_constant {cons_name=C_LOOP_STOP;arguments=[expr]}) loop_type
      ) |
      _ -> 
        let%bind expr = transpile_annotated_expression e in
        ok @@ Expression.make (E_constant {cons_name=C_LOOP_STOP;arguments=[expr]}) loop_type
  and matching : AST.expression_variable -> type_value -> bool -> AST.matching -> type_value -> expression result = fun fun_name loop_type shadowed m ty ->
    let return ret = ok @@ Expression.make ret @@ ty in
    let%bind expr = transpile_annotated_expression m.matchee in
    match m.cases with
      Match_bool {match_true; match_false} -> 
          let%bind (t , f) = bind_map_pair (replace_callback fun_name loop_type shadowed) (match_true, match_false) in
          return @@ E_if_bool (expr, t, f)
      | Match_option { match_none; match_some = {opt; body; tv} } ->
          let%bind n = replace_callback fun_name loop_type shadowed match_none in
          let%bind (tv' , s') =
            let%bind tv' = transpile_type tv in
            let%bind s' = replace_callback fun_name loop_type shadowed body in
            ok (tv' , s')
          in
          return @@ E_if_none (expr , n , ((opt , tv') , s'))
      | Match_list {
          match_nil ;
          match_cons = { hd ; tl ; body ; tv } ;
        } -> (
          let%bind nil = replace_callback fun_name loop_type shadowed match_nil in
          let%bind cons =
            let%bind ty' = transpile_type tv in
            let%bind match_cons' = replace_callback fun_name loop_type shadowed body in
            ok (((hd , ty') , (tl , ty')) , match_cons')
          in
          return @@ E_if_cons (expr , nil , cons)
        )
      | Match_variant {cases;tv} -> (
          let%bind tree =
            trace_strong (corner_case ~loc:__LOC__ "getting lr tree") @@
            tree_of_sum tv in
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
            | ((`Leaf (AST.Constructor constructor_name)) , tv) -> (
                let%bind {constructor=_ ; pattern ; body} =
                  trace_option (corner_case ~loc:__LOC__ "missing match clause") @@
                    let aux ({constructor = Constructor c ; pattern=_ ; body=_} : AST.matching_content_case) =
                      (c = constructor_name) in
                  List.find_opt aux cases in
                let%bind body' = replace_callback fun_name loop_type shadowed body in
                return @@ E_let_in ((pattern , tv) , false , top , body')
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
          aux expr tree''
       )
      | AST.Match_tuple _ -> failwith "match_tuple not supported"
  in
  let%bind fun_type = transpile_type fun_type in
  let%bind (input_type,output_type) = get_t_function fun_type in
  let loop_type = t_union (None, input_type) (None, output_type) in
  let%bind (body,binder) = map_lambda fun_name loop_type lambda.result in
  let binder = lambda.binder::binder in
  let%bind binder = match binder with hd::[] -> ok @@ hd | _ -> fail @@ unsupported_recursive_function fun_name in
  let expr = Expression.make_tpl (E_variable binder, input_type) in
  let body = Expression.make (E_iterator (C_LOOP_LEFT, ((lambda.binder, loop_type),body), expr)) output_type in
  ok @@ Expression.make (E_closure {binder;body}) fun_type

let transpile_declaration env (d:AST.declaration) : toplevel_statement result =
  match d with
  | Declaration_constant { binder ; expr ; inline ; post_env=_ } ->
      let%bind expression = transpile_annotated_expression expr in
      let tv = Combinators.Expression.get_type expression in
      let env' = Environment.add (binder, tv) env in
      ok @@ ((binder, inline, expression), environment_wrap env env')

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
