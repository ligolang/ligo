(* The Transpiler is a function that takes as input the Typed AST, and outputs expressions in a language that is basically a Michelson with named variables and first-class-environments.

For more info, see back-end.md: https://gitlab.com/ligolang/ligo/blob/dev/gitlab-pages/docs/contributors/big-picture/back-end.md *)

open Trace
open Helpers
module Errors = Errors
open Errors

module AST = Ast_typed
module Append_tree = Tree.Append
open AST.Combinators
open Mini_c

let untranspile = Untranspiler.untranspile

let temp_unwrap_loc = Location.unwrap
let temp_unwrap_loc_list = List.map Location.unwrap

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
  | C_CONVERT_TO_LEFT_COMB -> C_CONVERT_TO_LEFT_COMB
  | C_CONVERT_TO_RIGHT_COMB -> C_CONVERT_TO_RIGHT_COMB
  | C_CONVERT_FROM_LEFT_COMB -> C_CONVERT_FROM_LEFT_COMB
  | C_CONVERT_FROM_RIGHT_COMB -> C_CONVERT_FROM_RIGHT_COMB

let rec transpile_type (t:AST.type_expression) : (type_expression, transpiler_error) result =
  let return tc = ok @@ Expression.make_t ~loc:t.location @@ tc in
  match t.type_content with
  | T_variable (name) when Var.equal name Stage_common.Constant.t_bool -> return (T_base TB_bool)
  | t when (compare t (t_bool ()).type_content) = 0-> return (T_base TB_bool)
  | T_variable (name) -> fail @@ no_type_variable @@ name
  | T_constant (TC_int)       -> return (T_base TB_int)
  | T_constant (TC_nat)       -> return (T_base TB_nat)
  | T_constant (TC_mutez)     -> return (T_base TB_mutez)
  | T_constant (TC_string)    -> return (T_base TB_string)
  | T_constant (TC_bytes)     -> return (T_base TB_bytes)
  | T_constant (TC_address)   -> return (T_base TB_address)
  | T_constant (TC_timestamp) -> return (T_base TB_timestamp)
  | T_constant (TC_unit)      -> return (T_base TB_unit)
  | T_constant (TC_operation) -> return (T_base TB_operation)
  | T_constant (TC_signature) -> return (T_base TB_signature)
  | T_constant (TC_key)       -> return (T_base TB_key)
  | T_constant (TC_key_hash)  -> return (T_base TB_key_hash)
  | T_constant (TC_chain_id)  -> return (T_base TB_chain_id)
  | T_constant (TC_void)      -> return (T_base TB_void)
  | T_operator (TC_contract x) ->
      let%bind x' = transpile_type x in
      return (T_contract x')
  | T_operator (TC_map {k;v}) ->
      let%bind kv' = bind_map_pair transpile_type (k, v) in
      return (T_map kv')
  | T_operator (TC_big_map {k;v}) ->
      let%bind kv' = bind_map_pair transpile_type (k, v) in
      return (T_big_map kv')
  | T_operator (TC_map_or_big_map _) ->
      fail @@ corner_case ~loc:"transpiler" "TC_map_or_big_map should have been resolved before transpilation"
  | T_operator (TC_list t) ->
      let%bind t' = transpile_type t in
      return (T_list t')
  | T_operator (TC_set t) ->
      let%bind t' = transpile_type t in
      return (T_set t')
  | T_operator (TC_option o) ->
      let%bind o' = transpile_type o in
      return (T_option o')
  | T_sum m when Ast_typed.Helpers.is_michelson_or m ->
      let node = Append_tree.of_list @@ kv_list_of_cmap m in
      let aux a b : (type_expression annotated , transpiler_error) result =
        let%bind a = a in
        let%bind b = b in
        let%bind t = return @@ T_or (a,b) in
        ok (None, t)
      in
      let%bind m' = Append_tree.fold_ne
                      (fun (_, ({ctor_type ; michelson_annotation}: AST.ctor_content)) ->
                        let%bind a = transpile_type ctor_type in
                        ok (Ast_typed.Helpers.remove_empty_annotation michelson_annotation, a) )
                      aux node in
      ok @@ snd m'
  | T_sum m ->
      let node = Append_tree.of_list @@ kv_list_of_cmap m in
      let aux a b : (type_expression annotated , transpiler_error) result =
        let%bind a = a in
        let%bind b = b in
        let%bind t = return @@ T_or (a,b) in
        ok (None, t)
      in
      let%bind m' = Append_tree.fold_ne
                      (fun (Ast_typed.Types.Constructor ann, ({ctor_type ; _}: AST.ctor_content)) ->
                        let%bind a = transpile_type ctor_type in
                        ok (Some (String.uncapitalize_ascii ann), a))
                      aux node in
      ok @@ snd m'
  | T_record m when Ast_typed.Helpers.is_michelson_pair m ->
      let node = Append_tree.of_list @@ Ast_typed.Helpers.tuple_of_record m in
      let aux a b : (type_expression annotated , transpiler_error) result =
        let%bind a = a in
        let%bind b = b in
        let%bind t = return @@ T_pair (a, b) in
        ok (None, t)
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
      let aux a b : (type_expression annotated, transpiler_error) result =
        let%bind a = a in
        let%bind b = b in
        let%bind t = return @@ T_pair (a, b) in
        ok (None, t)
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
      return @@ (T_function (param',result'))
    )

let record_access_to_lr : type_expression -> type_expression AST.label_map -> AST.label -> ((type_expression * [`Left | `Right]) list , transpiler_error) result = fun ty tym ind ->
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
        trace_option (corner_case ~loc:__LOC__ "record access pair") @@
        Mini_c.get_t_pair ty in
      match cur with
      | `Left -> ok (a , acc @ [(a , `Left)])
      | `Right -> ok (b , acc @ [(b , `Right)] ) in
    bind_fold_list aux (ty , []) lr_path in
  ok lst

let rec transpile_literal : AST.literal -> value = fun l -> match l with
  | Literal_int n -> D_int n
  | Literal_nat n -> D_nat n
  | Literal_timestamp n -> D_timestamp n
  | Literal_mutez n -> D_mutez n
  | Literal_bytes s -> D_bytes s
  | Literal_string s -> D_string (Ligo_string.extract s)
  | Literal_address s -> D_string s
  | Literal_signature s -> D_string s
  | Literal_key s -> D_string s
  | Literal_key_hash s -> D_string s
  | Literal_chain_id s -> D_string s
  | Literal_operation op -> D_operation op
  | Literal_unit -> D_unit
  | Literal_void -> D_none

and tree_of_sum : AST.type_expression -> ((AST.constructor' * AST.type_expression) Append_tree.t, transpiler_error) result = fun t ->
  let%bind map_tv =
    trace_option (corner_case ~loc:__LOC__ "getting lr tree") @@
    get_t_sum t in
  let kt_list = List.map (fun (k,({ctor_type;_}:AST.ctor_content)) -> (k,ctor_type)) (kv_list_of_cmap map_tv) in
  ok @@ Append_tree.of_list kt_list

and transpile_annotated_expression (ae:AST.expression) : (expression , transpiler_error) result =
  let%bind tv = transpile_type ae.type_expression in
  let return ?(tv = tv) expr = ok @@ Combinators.Expression.make_tpl ~loc:ae.location (expr, tv) in
  trace (translation_tracer ae.location) @@
  match ae.expression_content with
  | E_let_in {let_binder; rhs; let_result; inline} ->
    let%bind rhs' = transpile_annotated_expression rhs in
    let%bind result' = transpile_annotated_expression let_result in
    return (E_let_in ((let_binder, rhs'.type_expression), inline, rhs', result'))
  | E_literal l -> return @@ E_literal (transpile_literal l)
  | E_variable name -> (
      return @@ E_variable (name)
    )
  | E_application {lamb; args} ->
      let%bind a = transpile_annotated_expression lamb in
      let%bind b = transpile_annotated_expression args in
      return @@ E_application (a, b)
  | E_constructor {constructor=Constructor name;element} when (String.equal name "true"|| String.equal name "false") && element.expression_content = AST.e_unit () ->
    return @@ E_literal (D_bool (bool_of_string name))
  | E_constructor {constructor;element} -> (
      let%bind param' = transpile_annotated_expression element in
      let (param'_expr , param'_tv) = Combinators.Expression.(get_content param' , get_type param') in
      let%bind node_tv =
        trace_strong (corner_case ~loc:__LOC__ "getting lr tree") @@
        tree_of_sum ae.type_expression in
      let leaf (k, tv) : (expression_content option * type_expression , transpiler_error) result =
        if k = constructor then (
          let%bind _ =
            trace_option (corner_case ~loc:__LOC__ "wrong type for constructor parameter")
            @@ AST.assert_type_expression_eq (tv, element.type_expression) in
          ok (Some (param'_expr), param'_tv)
        ) else (
          let%bind tv = transpile_type tv in
          ok (None, tv)
        ) in
      let node a b : (expression_content option * type_expression , transpiler_error) result =
        let%bind a = a in
        let%bind b = b in
        match (a, b) with
        | (None, a), (None, b) -> ok (None, Expression.make_t @@ T_or ((None, a), (None, b)))
        | (Some _, _), (Some _, _) -> fail @@ corner_case ~loc:__LOC__ "multiple identical constructors in the same variant"
        | (Some v, a), (None, b) -> ok (Some (E_constant {cons_name=C_LEFT ;arguments= [Combinators.Expression.make_tpl (v, a)]}), Expression.make_t @@ T_or ((None, a), (None, b)))
        | (None, a), (Some v, b) -> ok (Some (E_constant {cons_name=C_RIGHT;arguments= [Combinators.Expression.make_tpl (v, b)]}), Expression.make_t @@ T_or ((None, a), (None, b)))
      in
      let%bind (ae_opt, tv) = Append_tree.fold_ne leaf node node_tv in
      let%bind ae =
        trace_option (corner_case ~loc:__LOC__ "inexistant constructor")
          ae_opt in
      return ~tv ae
    )
  | E_record m -> (
      let node = Append_tree.of_list @@ Ast_typed.Helpers.list_of_record_or_tuple m in
      let aux a b : (expression , transpiler_error) result =
        let%bind a = a in
        let%bind b = b in
        let a_ty = Combinators.Expression.get_type a in
        let b_ty = Combinators.Expression.get_type b in
        let tv   = Combinators.Expression.make_t @@ T_pair ((None, a_ty) , (None, b_ty)) in
        return ~tv @@ E_constant {cons_name=C_PAIR;arguments=[a; b]}
      in
      trace_strong (corner_case ~loc:__LOC__ "record build") @@
      Append_tree.fold_ne (transpile_annotated_expression) aux node
    )
  | E_record_accessor {record; path} ->
      let%bind ty' = transpile_type (get_type_expression record) in
      let%bind ty_lmap =
        trace_option (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record (get_type_expression record) in
      let%bind ty'_lmap = Ast_typed.Helpers.bind_map_lmap_t transpile_type ty_lmap in
      let%bind path = record_access_to_lr ty' ty'_lmap path in
      let aux = fun pred (ty, lr) ->
        let c = match lr with
          | `Left  -> C_CAR
          | `Right -> C_CDR 
        in
        return ~tv:ty @@ E_constant {cons_name=c;arguments=[pred]} 
      in
      let%bind record' = transpile_annotated_expression record in
      let%bind expr = bind_fold_list aux record' path in
      ok expr
  | E_record_update {record; path; update} -> 
      let rec aux res (r,p,up) =
        let ty = get_type_expression r in
        let%bind ty_lmap =
          trace_option (corner_case ~loc:__LOC__ "not a record") @@
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
        let expression_to_iterator_body (f : AST.expression) =
          let%bind (input , output) = trace_option (corner_case ~loc:__LOC__ "expected function type") @@ AST.get_t_function f.type_expression in
          let%bind f' = transpile_annotated_expression f in
          let%bind input' = transpile_type input in
          let%bind output' = transpile_type output in
          let binder = Var.fresh ~name:"iterated" () in
          let application = Mini_c.Combinators.e_application f' output' (Mini_c.Combinators.e_var binder input') in
          ok ((binder , input'), application)
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
    let%bind io = trace_option (corner_case ~loc:__LOC__ "expected function type") @@
      AST.get_t_function ae.type_expression in
    transpile_lambda l io
  | E_recursive r ->
    transpile_recursive r
  | E_matching {matchee=expr; cases=m} -> (
      let%bind expr' = transpile_annotated_expression expr in
      match m with
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
      | Match_variant {cases=[{constructor=Constructor t;body=match_true};{constructor=Constructor f;body=match_false}];_}
        when String.equal t "true" && String.equal f "false" ->
          let%bind (t , f) = bind_map_pair (transpile_annotated_expression) (match_true, match_false) in
          return @@ E_if_bool (expr', t, f)
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
                  let%bind a_ty = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                  let left_var = Var.fresh ~name:"left" () in
                  let%bind e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                  ok ((left_var , a_ty) , e)
                in
                let%bind b' =
                  let%bind b_ty = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                  let right_var = Var.fresh ~name:"right" () in
                  let%bind e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                  ok ((right_var , b_ty) , e)
                in
                return @@ E_if_left (top , a' , b')
          in
          trace_strong (corner_case ~loc:__LOC__ "building constructor") @@
          aux expr' tree''
       )
  )
  | E_raw_code { language; code} -> 
    let backend = "Michelson" in
    let%bind () =
      Assert.assert_true
        (corner_case ~loc:__LOC__ "Language insert - backend mismatch only provide code insertion in the language you are compiling to")
        (String.equal language backend)
    in
    let type_anno  = get_type_expression code in
    let%bind type_anno' = transpile_type type_anno in
    let%bind code = trace_option (corner_case ~loc:__LOC__ "could not get a string") @@ get_a_string code in
    return ~tv:type_anno' @@ E_raw_michelson code

and transpile_lambda l (input_type , output_type) =
  let { binder ; result } : AST.lambda = l in
  let%bind result' = transpile_annotated_expression result in
  let%bind input = transpile_type input_type in
  let%bind output = transpile_type output_type in
  let tv = Combinators.t_function input output in
  let binder = binder in 
  let closure = E_closure { binder; body = result'} in
  ok @@ Combinators.Expression.make_tpl ~loc:result.location (closure , tv)

and transpile_recursive {fun_name; fun_type; lambda} =
  let rec map_lambda : AST.expression_variable -> type_expression -> AST.expression -> (expression * expression_variable list , transpiler_error) result = fun fun_name loop_type e ->
    match e.expression_content with 
      E_lambda {binder;result} -> 
        let%bind (body,l) = map_lambda fun_name loop_type result in
        ok @@ (Expression.make ~loc:e.location (E_closure {binder;body}) loop_type, binder::l)
      | _  -> 
        let%bind res = replace_callback fun_name loop_type false e in
        ok @@ (res, [])

  and replace_callback : AST.expression_variable -> type_expression -> bool -> AST.expression -> (expression , transpiler_error) result = fun fun_name loop_type shadowed e ->
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

  and matching : AST.expression_variable -> type_expression -> bool -> AST.matching -> type_expression -> (expression , transpiler_error) result = fun fun_name loop_type shadowed m ty ->
    let return ret = ok @@ Expression.make ret @@ ty in
    let%bind expr = transpile_annotated_expression m.matchee in
    match m.cases with
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
      | Match_variant {cases=[{constructor=Constructor t;body=match_true};{constructor=Constructor f;body=match_false}];_}
        when String.equal t "true" && String.equal f "false" ->
          let%bind (t , f) = bind_map_pair (replace_callback fun_name loop_type shadowed) (match_true, match_false) in
          return @@ E_if_bool (expr, t, f)
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
                  let%bind a_ty = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                  let left_var = Var.fresh ~name:"left" () in
                  let%bind e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                  ok ((left_var , a_ty) , e)
                in
                let%bind b' =
                  let%bind b_ty = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                  let right_var = Var.fresh ~name:"right" () in
                  let%bind e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                  ok ((right_var , b_ty) , e)
                in
                return @@ E_if_left (top , a' , b')
          in
          trace_strong (corner_case ~loc:__LOC__ "building constructor") @@
          aux expr tree''
       )
  in
  let%bind fun_type = transpile_type fun_type in
  let%bind (input_type,output_type) = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_function fun_type in
  let loop_type = t_union (None, input_type) (None, output_type) in
  let%bind (body,binder) = map_lambda fun_name loop_type lambda.result in
  let binder = lambda.binder::binder in
  let%bind binder = match binder with hd::[] -> ok @@ hd | _ -> fail @@ unsupported_recursive_function fun_name in
  let expr = Expression.make_tpl (E_variable binder, input_type) in
  let body = Expression.make (E_iterator (C_LOOP_LEFT, ((lambda.binder, loop_type),body), expr)) output_type in
  ok @@ Expression.make (E_closure {binder;body}) fun_type

let transpile_declaration env (d:AST.declaration) : (toplevel_statement option , transpiler_error) result =
  match d with
  | Declaration_constant { binder ; expr ; inline } ->
      let%bind expression = transpile_annotated_expression expr in
      let tv = Combinators.Expression.get_type expression in
      let env' = Environment.add (binder, tv) env in
      ok @@ Some ((binder, inline, expression), environment_wrap env env')
  | _ -> ok None

let transpile_program (lst : AST.program) : (program , transpiler_error) result =
  let aux (prev:(toplevel_statement list * Environment.t , transpiler_error) result) cur =
    let%bind (hds, env) = prev in
    match%bind transpile_declaration env cur with
    | Some ((_ , env')  as cur') -> ok (hds @ [ cur' ] , env'.post_environment)
    | None -> ok (hds , env)
  in
  let%bind (statements, _) = List.fold_left aux (ok ([], Environment.empty)) (temp_unwrap_loc_list lst) in
  ok statements

let extract_constructor (v : value) (tree : _ Append_tree.t') : (string * value * AST.type_expression , transpiler_error) result =
  let open Append_tree in
  let rec aux tv : (string * value * AST.type_expression , transpiler_error) result=
    match tv with
    | Leaf (k, t), v -> ok (k, v, t)
    | Node {a}, D_left v -> aux (a, v)
    | Node {b}, D_right v -> aux (b, v)
    | _ -> fail (corner_case ~loc:__LOC__ "extract constructor")
  in
  let%bind (s, v, t) = aux (tree, v) in
  ok (s, v, t)

let extract_tuple (v : value) (tree : AST.type_expression Append_tree.t') : ((value * AST.type_expression) list , transpiler_error) result =
  let open Append_tree in
  let rec aux tv : ((value * AST.type_expression) list , transpiler_error) result =
    match tv with
    | Leaf t, v -> ok @@ [v, t]
    | Node {a;b}, D_pair (va, vb) ->
        let%bind a' = aux (a, va) in
        let%bind b' = aux (b, vb) in
        ok (a' @ b')
    | _ -> fail (corner_case ~loc:__LOC__ "extract tuple")
  in
  aux (tree, v)

let extract_record (v : value) (tree : _ Append_tree.t') : (_ list , transpiler_error) result =
  let open Append_tree in
  let rec aux tv : ((string * (value * AST.type_expression)) list , transpiler_error) result =
    match tv with
    | Leaf (s, t), v -> ok @@ [s, (v, t)]
    | Node {a;b}, D_pair (va, vb) ->
        let%bind a' = aux (a, va) in
        let%bind b' = aux (b, vb) in
        ok (a' @ b')
    | _ -> fail (corner_case ~loc:__LOC__ "bad record path")
  in
  aux (tree, v)
