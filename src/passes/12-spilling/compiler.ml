(* The compiler is a function that takes as input the Typed AST, and outputs expressions in a language that is basically a Michelson with named variables and first-class-environments.

   For more info, see back-end.md: https://gitlab.com/ligolang/ligo/blob/dev/gitlab-pages/docs/contributors/big-picture/back-end.md *)

open Trace
module Errors = Errors
open Errors

module AST = Ast_typed
module Append_tree = Tree.Append
open AST.Combinators
open Mini_c

module SMap = Map.Make(String)

let temp_unwrap_loc = Location.unwrap
let temp_unwrap_loc_list = List.map Location.unwrap

let compile_constant' : AST.constant' -> constant' = function
  | C_INT -> C_INT
  | C_UNIT -> C_UNIT
  | C_NIL -> C_NIL
  | C_NOW -> C_NOW
  | C_IS_NAT -> C_IS_NAT
  | C_SOME -> C_SOME
  | C_NONE -> C_NONE
  | C_ASSERTION -> C_ASSERTION
  | C_ASSERT_SOME -> C_ASSERT_SOME
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
  | C_FOLD_LEFT -> C_FOLD_LEFT
  | C_FOLD_RIGHT -> C_FOLD_RIGHT
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
  | C_TRUE -> C_TRUE
  | C_FALSE -> C_FALSE
  (* Set *)
  | C_SET_EMPTY -> C_SET_EMPTY
  | C_SET_LITERAL -> C_SET_LITERAL
  | C_SET_ADD -> C_SET_ADD
  | C_SET_REMOVE -> C_SET_REMOVE
  | C_SET_ITER -> C_SET_ITER
  | C_SET_FOLD -> C_SET_FOLD
  | C_SET_FOLD_DESC -> C_SET_FOLD_DESC
  | C_SET_MEM -> C_SET_MEM
  | C_SET_UPDATE -> C_SET_UPDATE
  (* List *)
  | C_LIST_EMPTY -> C_LIST_EMPTY
  | C_LIST_LITERAL -> C_LIST_LITERAL
  | C_LIST_ITER -> C_LIST_ITER
  | C_LIST_MAP -> C_LIST_MAP
  | C_LIST_FOLD -> C_LIST_FOLD
  | C_LIST_FOLD_LEFT -> C_LIST_FOLD_LEFT
  | C_LIST_FOLD_RIGHT -> C_LIST_FOLD_RIGHT
  | C_LIST_HEAD_OPT -> C_LIST_HEAD_OPT
  | C_LIST_TAIL_OPT -> C_LIST_TAIL_OPT
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
  | C_SHA3 -> C_SHA3
  | C_KECCAK -> C_KECCAK
  | C_LEVEL -> C_LEVEL
  | C_VOTING_POWER -> C_VOTING_POWER 
  | C_TOTAL_VOTING_POWER -> C_TOTAL_VOTING_POWER
  | C_TICKET -> C_TICKET
  | C_READ_TICKET -> C_READ_TICKET
  | C_SPLIT_TICKET -> C_SPLIT_TICKET
  | C_JOIN_TICKET -> C_JOIN_TICKET
  | C_PAIRING_CHECK -> C_PAIRING_CHECK
  | C_MAP_GET_AND_UPDATE -> C_MAP_GET_AND_UPDATE
  | C_BIG_MAP_GET_AND_UPDATE -> C_BIG_MAP_GET_AND_UPDATE
  | C_SAPLING_EMPTY_STATE -> C_SAPLING_EMPTY_STATE
  | C_SAPLING_VERIFY_UPDATE -> C_SAPLING_VERIFY_UPDATE
  | C_POLYMORPHIC_ADD -> C_POLYMORPHIC_ADD
  | (   C_TEST_ORIGINATE
      | C_TEST_SET_NOW
      | C_TEST_SET_SOURCE
      | C_TEST_SET_BAKER
      | C_TEST_EXTERNAL_CALL
      | C_TEST_EXTERNAL_CALL_EXN
      | C_TEST_GET_STORAGE
      | C_TEST_GET_BALANCE
      | C_TEST_MICHELSON_EQUAL
      | C_TEST_LOG 
      | C_TEST_COMPILE_EXPRESSION
      | C_TEST_COMPILE_EXPRESSION_SUBST
      | C_TEST_GET_NTH_BS
      | C_TEST_STATE_RESET
      | C_TEST_LAST_ORIGINATIONS
      | C_TEST_COMPILE_META_VALUE ) as c ->
    failwith (Format.asprintf "%a is only available for LIGO interpreter" PP.constant c)

let rec compile_type (t:AST.type_expression) : (type_expression, spilling_error) result =
  let return tc = ok @@ Expression.make_t ~loc:t.location @@ tc in
  match t.type_content with
  | T_variable (name) -> fail @@ no_type_variable @@ name
  | t when (AST.Compare.type_content t (t_bool ()).type_content) = 0-> return (T_base TB_bool)
  | T_constant {language ; injection ; parameters} -> (
    let open Stage_common.Constant in
    let%bind () = Assert.assert_true (corner_case ~loc:__LOC__ "unsupported language") @@ String.equal language Stage_common.Backends.michelson in
    match Ligo_string.extract injection , parameters with
    | (i, []) when String.equal i bool_name -> return (T_base TB_bool)
    | (i, []) when String.equal i unit_name -> return (T_base TB_unit)
    | (i, []) when String.equal i int_name -> return (T_base TB_int)
    | (i, []) when String.equal i nat_name -> return (T_base TB_nat)
    | (i, []) when String.equal i timestamp_name -> return (T_base TB_timestamp)
    | (i, []) when String.equal i tez_name -> return (T_base TB_mutez)
    | (i, []) when String.equal i string_name -> return (T_base TB_string)
    | (i, []) when String.equal i bytes_name -> return (T_base TB_bytes)
    | (i, []) when String.equal i address_name -> return (T_base TB_address)
    | (i, []) when String.equal i operation_name -> return (T_base TB_operation)
    | (i, []) when String.equal i key_name -> return (T_base TB_key)
    | (i, []) when String.equal i key_hash_name -> return (T_base TB_key_hash)
    | (i, []) when String.equal i chain_id_name -> return (T_base TB_chain_id)
    | (i, []) when String.equal i signature_name -> return (T_base TB_signature)
    | (i, []) when String.equal i baker_hash_name -> return (T_base TB_baker_hash)
    | (i, []) when String.equal i pvss_key_name -> return (T_base TB_pvss_key)

    | (i, []) when String.equal i baker_operation_name -> return (T_base TB_baker_operation)
    | (i, []) when String.equal i bls12_381_g1_name -> return (T_base TB_bls12_381_g1)
    | (i, []) when String.equal i bls12_381_g2_name -> return (T_base TB_bls12_381_g2)
    | (i, []) when String.equal i bls12_381_fr_name -> return (T_base TB_bls12_381_fr)
    | (i, []) when String.equal i never_name -> return (T_base TB_never)
    | (i, [x]) when String.equal i ticket_name ->
      let%bind x' = compile_type x in
      return (T_ticket x')
    | (i, [x]) when String.equal i sapling_transaction_name -> (
      match x.type_content with
      | AST.T_singleton (Literal_int x') -> return (T_sapling_transaction x')
      | _ -> failwith "wrong sapling_transaction"
    )
    | (i, [x]) when String.equal i sapling_state_name -> (
      match x.type_content with
      | AST.T_singleton (Literal_int x') -> return (T_sapling_state x')
      | _ -> failwith "wrong sapling_state"
    )
    | (i, [x]) when String.equal i contract_name ->
      let%bind x' = compile_type x in
      return (T_contract x')
    | (i, [o]) when String.equal i option_name ->
      let%bind o' = compile_type o in
      return (T_option o')
    | (i, [k;v]) when String.equal i map_name ->
      let%bind kv' = bind_map_pair compile_type (k, v) in
      return (T_map kv')
    | (i, [k; v]) when String.equal i big_map_name ->
      let%bind kv' = bind_map_pair compile_type (k, v) in
      return (T_big_map kv')
    | (i, _) when String.equal i map_or_big_map_name ->
      fail @@ corner_case ~loc:"spilling" "TC_map_or_big_map should have been resolved before spilling"
    | (i, [t]) when String.equal i list_name ->
      let%bind t' = compile_type t in
      return (T_list t')
    | (i, [t]) when String.equal i set_name ->
      let%bind t' = compile_type t in
      return (T_set t')
    | _ -> fail @@ corner_case ~loc:__LOC__ "wrong constant"
  )
  | T_sum { content = m ; layout } -> (
      let open Ast_typed.Helpers in
      match is_michelson_or m with
      | Some (a , b) -> (
          let aux (x : AST.row_element) =
            let%bind t = compile_type x.associated_type in
            let annot = remove_empty_annotation x.michelson_annotation in
            ok (annot , t)
          in
          let%bind a' = aux a in
          let%bind b' = aux b in
          return @@ T_or (a' , b')
        )
      | None -> Layout.t_sum ~layout return compile_type m
    )
  | T_record { content = m ; layout } -> (
      let open Ast_typed.Helpers in
      match is_michelson_pair m with
      | Some (a , b) -> (
          let aux (x : AST.row_element) =
            let%bind t = compile_type x.associated_type in
            let annot = remove_empty_annotation x.michelson_annotation in
            ok (annot , t)
          in
          let%bind a' = aux a in
          let%bind b' = aux b in
          let t = T_pair (a' , b') in
          return t
        )
      | None -> Layout.t_record_to_pairs ~layout return compile_type m
    )
  | T_arrow {type1;type2} -> (
      let%bind param' = compile_type type1 in
      let%bind result' = compile_type type2 in
      return @@ (T_function (param',result'))
  )
  | T_module_accessor _ ->
    fail @@ corner_case ~loc:__LOC__ "Module access should de resolved earlier"
  | T_singleton _ ->
    fail @@ corner_case ~loc:__LOC__ "Singleton uncaught"

(* probably should use result monad for conformity? but these errors
   are supposed to be impossible *)
let internal_error loc msg =
  failwith
    (Format.asprintf
       "@[<v>Internal error, please report this as a bug@ %s@ %s@ @]"
       loc msg)

let rec build_record_accessor record path =
  match path with
  | [] -> ok record
  | label :: path ->
    let%bind t =
      trace_option (corner_case ~loc:__LOC__ "Could not get record field type")
      @@ get_record_field_type (get_type_expression record) label in
    build_record_accessor
      (Ast_typed.Combinators.make_e
         (E_record_accessor { record = record ; path = label })
         t)
      path

let rec compile_literal : AST.literal -> value = fun l -> match l with
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

and compile_expression ?(module_env = SMap.empty) (ae:AST.expression) : (expression , spilling_error) result =
  let%bind tv = compile_type ae.type_expression in
  let self ?(module_env=module_env) = compile_expression ~module_env in
  let return ?(tv = tv) expr =
    ok @@ Combinators.Expression.make_tpl ~loc:ae.location (expr, tv) in
  match ae.expression_content with
  | E_let_in {let_binder; rhs; let_result; inline} ->
    let%bind rhs' = self rhs in
    let%bind result' = self let_result in
    return (E_let_in (rhs', inline, ((Location.map Var.todo_cast let_binder, rhs'.type_expression), result')))
  | E_type_in {type_binder=_; rhs=_; let_result} ->
    let%bind result' = self let_result in
    ok result'
  | E_mod_in {module_binder; rhs; let_result} ->
    let%bind record,module_env = compile_module_as_record module_binder module_env rhs in
    let t_record = record.type_expression in
    let%bind result' = self ~module_env let_result in
    return @@ E_let_in (record,false, ((Location.wrap @@ Var.of_name module_binder, t_record),result'))
  | E_mod_alias {alias; binders; result} -> (
    let module_binder,access = binders in
    let%bind module_ =
      trace_option (corner_case ~loc:__LOC__ "Mod_alias: This program shouldn't type")
      @@ SMap.find_opt  module_binder module_env  in
    let module_var = e_a_variable (Location.wrap (Var.of_name module_binder)) module_ in
    let%bind module_expr = build_record_accessor module_var (List.map (fun l -> Label l) access) in
    let module_ = get_type_expression module_expr in
    let%bind module_expr = self ~module_env module_expr in
    let module_env  = SMap.add alias module_ module_env in
    let%bind result' = self ~module_env result in
    return @@ E_let_in (module_expr,false,((Location.wrap @@ Var.of_name alias, tv),result'))
  )
  | E_literal l -> return @@ E_literal l
  | E_variable name -> (
      return @@ E_variable (Location.map Var.todo_cast name)
    )
  | E_application {lamb; args} ->
      let%bind a = self lamb in
      let%bind b = self args in
      return @@ E_application (a, b)
  | E_constructor {constructor=Label name;element} when (String.equal name "true"|| String.equal name "false") && element.expression_content = AST.e_unit () ->
    return @@ E_constant { cons_name = if bool_of_string name then C_TRUE else C_FALSE ; arguments = [] }
  | E_constructor {constructor;element} -> (
    let%bind ty' = compile_type ae.type_expression in
    let%bind ty_variant =
      trace_option (corner_case ~loc:__LOC__ "not a record") @@
      get_t_sum (get_type_expression ae) in
    let%bind path = Layout.constructor_to_lr ~layout:ty_variant.layout ty' ty_variant.content constructor in
    let aux = fun pred (ty, lr) ->
      let c = match lr with
        | `Left  -> C_LEFT
        | `Right -> C_RIGHT
      in
      return ~tv:ty @@ E_constant {cons_name=c;arguments=[pred]}
    in
    let%bind element' = self element in
    let%bind expr = bind_fold_list aux element' path in
    ok expr
  )
  | E_record m -> (
      let%bind record_t = trace_option (corner_case ~loc:__LOC__ "record expected") (AST.get_t_record ae.type_expression) in
      Layout.record_to_pairs self return record_t m
    )
  | E_record_accessor {record; path} ->
    let%bind ty' = compile_type (get_type_expression record) in
    let%bind {content ; layout} = trace_option (corner_case ~loc:__LOC__ "not a record") @@
      get_t_record (get_type_expression record) in
    let%bind path = Layout.record_access_to_lr ~layout ty' content path in
    let aux = fun pred (ty, lr) ->
      let c = match lr with
        | `Left  -> C_CAR
        | `Right -> C_CDR
      in
      return ~tv:ty @@ E_constant {cons_name=c;arguments=[pred]}
    in
    let%bind record' = compile_expression record in
    let%bind expr = bind_fold_list aux record' path in
    ok expr
  | E_record_update {record; path; update} ->
    (* Compile record update to simple constructors &
       projections. This will be optimized to some degree by eta
       contraction in a later pass. *)

    let rec aux res (r,p,up) =
      let ty = get_type_expression r in
      let%bind {content;layout} =
        trace_option (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record (ty) in
      let%bind ty' = compile_type (ty) in
      let%bind p' =
        trace_strong (corner_case ~loc:__LOC__ "record access") @@
        Layout.record_access_to_lr ~layout ty' content p in
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
      let%bind update = self update in
      let%bind record = self record in
      let record_var = Var.fresh () in
      let car (e : expression) : expression =
        match e.type_expression.type_content with
        | T_pair ((_, a), _) ->
          { e with
            content = E_constant { cons_name = C_CAR ; arguments = [e] } ;
            type_expression = a }
        | _ -> internal_error __LOC__ "record did not have pair type" in
      let cdr (e : expression) : expression =
        match e.type_expression.type_content with
        | T_pair (_, (_, b)) ->
          { e with
            content = E_constant { cons_name = C_CDR ; arguments = [e] } ;
            type_expression = b }
        | _ -> internal_error __LOC__ "record did not have pair type" in
      let rec build_record_update record path =
        match path with
        | [] -> update
        | `Left :: path ->
          { record with
            content = E_constant { cons_name = C_PAIR ;
                                   arguments = [ build_record_update (car record) path;
                                                 cdr record ] } }
        | `Right :: path ->
          { record with
            content = E_constant { cons_name = C_PAIR ;
                                   arguments = [ car record;
                                                 build_record_update (cdr record) path ] } } in
      return
        (E_let_in (record, false, ((Location.wrap record_var, record.type_expression),
                   build_record_update
                     (e_var (Location.wrap record_var) record.type_expression)
                     path)))
  | E_constant {cons_name=name; arguments=lst} -> (
      let iterator_generator iterator_name =
        let expression_to_iterator_body (f : AST.expression) =
          let%bind (input , output) = trace_option (corner_case ~loc:__LOC__ "expected function type") @@ AST.get_t_function f.type_expression in
          let%bind f' = self f in
          let%bind input' = compile_type input in
          let%bind output' = compile_type output in
          let binder = Location.wrap @@ Var.fresh ~name:"iterated" () in
          let application = Mini_c.Combinators.e_application f' output' (Mini_c.Combinators.e_var binder input') in
          ok ((binder , input'), application)
        in
        fun (lst : AST.expression list) -> match (lst , iterator_name) with
          | [f ; i] , C_ITER | [f ; i] , C_MAP -> (
              let%bind f' = expression_to_iterator_body f in
              let%bind i' = self i in
              return @@ E_iterator (iterator_name , f' , i')
            )
          | [ f ; collection ; initial ] , C_FOLD -> (
              let%bind f' = expression_to_iterator_body f in
              let%bind initial' = self initial in
              let%bind collection' = self collection in
              return @@ E_fold (f' , collection' , initial')
            )
          | [ f ; initial ; collection ], C_FOLD_LEFT -> (
              let%bind f' = expression_to_iterator_body f in
              let%bind initial' = self initial in
              let%bind collection' = self collection in
              return @@ E_fold (f' , collection' , initial')
            )
          | [ f ; collection ; initial ], C_FOLD_RIGHT -> (
              let%bind f' = expression_to_iterator_body f in
              let%bind initial' = self initial in
              let%bind elem_type = 
                bind (trace_option (corner_case ~loc:__LOC__ "Wrong type : expecting collection")) @@
                map get_t_collection @@ compile_type collection.type_expression in
              let%bind collection' = self collection in
              return @@ E_fold_right (f' , (collection',elem_type) , initial')
            )
          | _ -> fail @@ corner_case ~loc:__LOC__ (Format.asprintf "bad iterator arity: %a" PP.constant iterator_name)
      in
      let (iter , map , fold, fold_left, fold_right) = iterator_generator C_ITER,
                                                       iterator_generator C_MAP,
                                                       iterator_generator C_FOLD,
                                                       iterator_generator C_FOLD_LEFT,
                                                       iterator_generator C_FOLD_RIGHT in
      match (name , lst) with
      | (C_SET_ITER , lst) -> iter lst
      | (C_LIST_ITER , lst) -> iter lst
      | (C_MAP_ITER , lst) -> iter lst
      | (C_LIST_MAP , lst) -> map lst
      | (C_MAP_MAP , lst) -> map lst
      | (C_LIST_FOLD , lst) -> fold lst
      | (C_SET_FOLD , lst) -> fold lst
      | (C_MAP_FOLD , lst) -> fold lst
      | (C_FOLD, lst) -> fold lst
      | (C_LIST_FOLD_LEFT, lst) -> fold_left lst
      | (C_LIST_FOLD_RIGHT, lst) -> fold_right lst
      | (C_SET_FOLD_DESC , lst) -> fold_right lst
      | _ -> (
          let%bind lst' = bind_map_list (self) lst in
          return @@ E_constant {cons_name=compile_constant' name;arguments=lst'}
        )
    )
  | E_lambda l ->
    let%bind io = trace_option (corner_case ~loc:__LOC__ "expected function type") @@
      AST.get_t_function ae.type_expression in
    compile_lambda module_env l io
  | E_recursive r ->
    compile_recursive module_env r
  | E_matching {matchee=expr; cases=m} -> (
      let%bind expr' = self expr in
      match m with
      | Match_option { match_none; match_some = {opt; body; tv} } ->
          let%bind n = self match_none in
          let%bind (tv' , s') =
            let%bind tv' = compile_type tv in
            let%bind s' = self body in
            ok (tv' , s')
          in
          return @@ E_if_none (expr' , n , ((Location.map Var.todo_cast opt , tv') , s'))
      | Match_list {
          match_nil ;
          match_cons = {hd; tl; body; tv} ;
        } -> (
          let%bind nil = self match_nil in
          let%bind cons =
            let%bind elt_ty = compile_type tv in
            let list_ty = { type_content = T_list elt_ty ; location = Location.generated } in
            let%bind match_cons' = self body in
            ok (((Location.map Var.todo_cast hd , elt_ty) , (Location.map Var.todo_cast tl , list_ty)) , match_cons')
          in
          return @@ E_if_cons (expr' , nil , cons)
        )
      | Match_variant {cases ; tv} -> (
          match expr'.type_expression.type_content with
          | T_base TB_bool ->
            let ctor_body (case : AST.matching_content_case) = (case.constructor, case.body) in
            let cases = AST.LMap.of_list (List.map ctor_body cases) in
            let get_case c =
              trace_option
                (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
                (AST.LMap.find_opt (Label c) cases) in
            let%bind match_true  = get_case "true" in
            let%bind match_false = get_case "false" in
            let%bind (t , f) = bind_map_pair (self) (match_true, match_false) in
            return @@ E_if_bool (expr', t, f)
          | _ -> (
              let%bind { content ; layout } = trace_option (corner_case ~loc:__LOC__ "getting lr tree") @@
                get_t_sum tv in
              let%bind tree = Layout.match_variant_to_tree ~layout ~compile_type content in
              let rec aux top t =
                match t with
                | ((`Leaf (Label constructor_name)) , tv) -> (
                    let%bind {constructor=_ ; pattern ; body} =
                      trace_option (corner_case ~loc:__LOC__ "missing match clause") @@
                      let aux ({constructor = Label c ; pattern=_ ; body=_} : AST.matching_content_case) =
                        (c = constructor_name) in
                      List.find_opt aux cases in
                    let%bind body' = self body in
                    return @@ E_let_in (top, false, ((Location.map Var.todo_cast pattern , tv) , body'))
                  )
                | ((`Node (a , b)) , tv) ->
                  let%bind a' =
                    let%bind a_ty = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                    let left_var = Location.wrap @@ Var.fresh ~name:"left" () in
                    let%bind e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                    ok ((left_var , a_ty) , e)
                  in
                  let%bind b' =
                    let%bind b_ty = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                    let right_var = Location.wrap @@ Var.fresh ~name:"right" () in
                    let%bind e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                    ok ((right_var , b_ty) , e)
                  in
                  return @@ E_if_left (top , a' , b')
              in
              trace_strong (corner_case ~loc:__LOC__ "building constructor") @@
              aux expr' tree
            )
      )
      | Match_record { fields; body; tv } ->
        let%bind { content ; layout } = trace_option (corner_case ~loc:__LOC__ "getting lr tree") @@
          get_t_record tv in
        match layout with
        (* TODO unify with or simplify other case below? *)
        | L_comb ->
          let record_fields = Ast_typed.Helpers.kv_list_of_t_record_or_tuple ~layout:L_comb content in
          let%bind fields =
            bind_map_list
              (fun (l, (row_element : _ row_element_mini_c)) ->
                 let%bind t = compile_type row_element.associated_type in
                 ok (fst (LMap.find l fields), t))
              record_fields in
          let%bind body = self body in
          return (E_let_tuple (expr', (fields, body)))
        | _ ->
        let%bind tree = Layout.record_tree ~layout compile_type content in
        let%bind body = self body in
        let rec aux expr (tree : Layout.record_tree) body =
          match tree.content with
          | Field l ->
            let var = fst (LMap.find l fields) in
            return @@ E_let_in (expr, false, ((var, tree.type_), body))
          | Pair (x, y) ->
            let x_var = Location.wrap (Var.fresh ()) in
            let y_var = Location.wrap (Var.fresh ()) in
            let x_ty = x.type_ in
            let y_ty = y.type_ in
            let x_var_expr = Combinators.Expression.make_tpl (E_variable x_var, x_ty) in
            let y_var_expr = Combinators.Expression.make_tpl (E_variable y_var, y_ty) in
            let%bind yrec = aux y_var_expr y body in
            let%bind xrec = aux x_var_expr x yrec in
            return @@ E_let_tuple (expr, ([(x_var, x_ty); (y_var, y_ty)], xrec))
        in
        aux expr' tree body
  )
  | E_raw_code { language; code} ->
    let backend = Stage_common.Backends.michelson in
    let%bind () =
      Assert.assert_true
        (corner_case ~loc:__LOC__ "Language insert - backend mismatch only provide code insertion in the language you are compiling to")
        (String.equal language backend)
    in
    let type_anno  = get_type_expression code in
    let%bind type_anno' = compile_type type_anno in
    let%bind code = trace_option (corner_case ~loc:__LOC__ "could not get a string") @@ get_a_string code in
    let open Tezos_micheline in
    let orig_code = code in
    let (code, errs) = Micheline_parser.tokenize code in
    (match errs with
    | _ :: _ -> fail (could_not_parse_raw_michelson ae.location orig_code)
    | [] ->
      let (code, errs) = Micheline_parser.parse_expression ~check:false code in
      match errs with
      | _ :: _ -> fail (could_not_parse_raw_michelson ae.location orig_code)
      | [] ->
        let code = Micheline.strip_locations code in
        (* hmm *)
        let code = Micheline.inject_locations (fun _ -> Location.generated) code in
        match code with
        | Seq (_, code) ->
          return ~tv:type_anno' @@ E_raw_michelson code
        | _ ->
          fail (raw_michelson_must_be_seq ae.location code)
    )
  | E_module_accessor {module_name; element} -> (
    let module_var = module_name in
    let%bind module_ =
      trace_option (corner_case ~loc:__LOC__ "Module_accessor: This program shouldn't type")
      @@ SMap.find_opt module_var module_env in
    (* TODO E_module_accessor should not be this way *)
    let rec aux (element : AST.expression) =
      match element.expression_content with
      | E_module_accessor {module_name; element} ->
        let module_var = module_name in
        let%bind module_names = aux element in
        ok @@ (module_var :: module_names)
      | E_variable var ->
        ok @@ [Var.to_name @@ Location.unwrap @@ var]
      | E_record_accessor {record; path} ->
        let%bind module_names = aux record in
        let Label module_var = path in
        ok @@ (module_names @ [module_var])
      | _ -> fail @@ corner_case ~loc:__LOC__ "The parser shouldn't allowed this construct"
    in
    let%bind access_list = aux element in
    let module_var = e_a_variable (Location.wrap (Var.of_name module_var)) module_ in
    let%bind expr = build_record_accessor module_var (List.map (fun l -> Label l) access_list) in
    let%bind expr = self ~module_env expr in
    ok @@ expr
  )

and compile_lambda module_env l (input_type , output_type) =
  let { binder ; result } : AST.lambda = l in
  let%bind result' = compile_expression ~module_env result in
  let%bind input = compile_type input_type in
  let%bind output = compile_type output_type in
  let tv = Combinators.t_function input output in
  let binder = Location.map Var.todo_cast binder in
  let closure = E_closure { binder; body = result'} in
  ok @@ Combinators.Expression.make_tpl ~loc:result.location (closure , tv)

and compile_recursive module_env {fun_name; fun_type; lambda} =
  let rec map_lambda : AST.expression_variable -> type_expression -> AST.expression -> (expression * expression_variable list , spilling_error) result = fun fun_name loop_type e ->
    match e.expression_content with
      E_lambda {binder;result} ->
      let binder = Location.map Var.todo_cast binder in
      let%bind (body,l) = map_lambda fun_name loop_type result in
      ok @@ (Expression.make ~loc:e.location (E_closure {binder;body}) loop_type, binder::l)
    | _  ->
      let%bind res = replace_callback fun_name loop_type false e in
      ok @@ (res, [])

  and replace_callback : AST.expression_variable -> type_expression -> bool -> AST.expression -> (expression , spilling_error) result = fun fun_name loop_type shadowed e ->
    match e.expression_content with
      E_let_in li ->
        let shadowed = shadowed || Var.equal li.let_binder.wrap_content fun_name.wrap_content in
        let%bind let_result = replace_callback fun_name loop_type shadowed li.let_result in
        let%bind rhs = compile_expression ~module_env li.rhs in
        let%bind ty  = compile_type li.rhs.type_expression in
        ok @@ e_let_in (Location.map Var.todo_cast li.let_binder) ty li.inline rhs let_result |
      E_matching m ->
      let%bind ty = compile_type e.type_expression in
      matching fun_name loop_type shadowed m ty |
      E_application {lamb;args} -> (
        match lamb.expression_content,shadowed with
        E_variable name, false when Var.equal fun_name.wrap_content name.wrap_content ->
          let%bind expr = compile_expression ~module_env args in
          ok @@ Expression.make (E_constant {cons_name=C_LOOP_CONTINUE;arguments=[expr]}) loop_type |
        _ ->
          let%bind expr = compile_expression ~module_env e in
          ok @@ Expression.make (E_constant {cons_name=C_LOOP_STOP;arguments=[expr]}) loop_type
      ) |
      _ ->
        let%bind expr = compile_expression ~module_env e in
        ok @@ Expression.make (E_constant {cons_name=C_LOOP_STOP;arguments=[expr]}) loop_type

  and matching : AST.expression_variable -> type_expression -> bool -> AST.matching -> type_expression -> (expression , spilling_error) result = fun fun_name loop_type shadowed m ty ->
    let return ret = ok @@ Expression.make ret @@ ty in
    let%bind expr = compile_expression ~module_env m.matchee in
    match m.cases with
      | Match_option { match_none; match_some = {opt; body; tv} } ->
          let%bind n = replace_callback fun_name loop_type shadowed match_none in
          let%bind (tv' , s') =
            let%bind tv' = compile_type tv in
            let%bind s' = replace_callback fun_name loop_type shadowed body in
            ok (tv' , s')
          in
          return @@ E_if_none (expr , n , ((Location.map Var.todo_cast opt , tv') , s'))
      | Match_list {
          match_nil ;
          match_cons = { hd ; tl ; body ; tv } ;
        } -> (
          let%bind nil = replace_callback fun_name loop_type shadowed match_nil in
          let%bind cons =
            let%bind elt_ty = compile_type tv in
            let list_ty = { type_content = T_list elt_ty ; location = Location.generated } in
            let%bind match_cons' = replace_callback fun_name loop_type shadowed body in
            ok (((Location.map Var.todo_cast hd , elt_ty) , (Location.map Var.todo_cast tl , list_ty)) , match_cons')
          in
          return @@ E_if_cons (expr , nil , cons)
        )
      | Match_variant {cases=[{constructor=Label t;body=match_true};{constructor=Label f;body=match_false}];_}
        when String.equal t "true" && String.equal f "false" ->
          let%bind (t , f) = bind_map_pair (replace_callback fun_name loop_type shadowed) (match_true, match_false) in
          return @@ E_if_bool (expr, t, f)
      | Match_variant {cases;tv} -> (
          let%bind { content ; layout } = trace_option (corner_case ~loc:__LOC__ "getting lr tree") @@
            get_t_sum tv in
          let%bind tree = Layout.match_variant_to_tree ~layout ~compile_type content in
          let rec aux top t =
            match t with
            | ((`Leaf (Label constructor_name)) , tv) -> (
                let%bind {constructor=_ ; pattern ; body} =
                  trace_option (corner_case ~loc:__LOC__ "missing match clause") @@
                    let aux ({constructor = Label c ; pattern=_ ; body=_} : AST.matching_content_case) =
                      (c = constructor_name) in
                  List.find_opt aux cases in
                let%bind body' = replace_callback fun_name loop_type shadowed body in
                return @@ E_let_in (top, false, ((Location.map Var.todo_cast pattern , tv) , body'))
              )
            | ((`Node (a , b)) , tv) ->
                let%bind a' =
                  let%bind a_ty = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                  let left_var = Location.wrap @@ Var.fresh ~name:"left" () in
                  let%bind e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                  ok ((left_var , a_ty) , e)
                in
                let%bind b' =
                  let%bind b_ty = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                  let right_var = Location.wrap @@ Var.fresh ~name:"right" () in
                  let%bind e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                  ok ((right_var , b_ty) , e)
                in
                return @@ E_if_left (top , a' , b')
          in
          trace_strong (corner_case ~loc:__LOC__ "building constructor") @@
          aux expr tree
       )
      | Match_record { fields ; body ; tv } ->
        let%bind { content ; layout } = trace_option (corner_case ~loc:__LOC__ "getting lr tree") @@
            get_t_record tv in
        let%bind tree = Layout.record_tree ~layout compile_type content in
        let%bind body = replace_callback fun_name loop_type shadowed body in
        let rec aux expr (tree : Layout.record_tree) body =
          match tree.content with
          | Field l ->
            let var = fst (LMap.find l fields) in
            return @@ E_let_in (expr, false, ((var, tree.type_), body))
          | Pair (x, y) ->
            let x_var = Location.wrap (Var.fresh ()) in
            let y_var = Location.wrap (Var.fresh ()) in
            let x_ty = x.type_ in
            let y_ty = y.type_ in
            let x_var_expr = Combinators.Expression.make_tpl (E_variable x_var, x_ty) in
            let y_var_expr = Combinators.Expression.make_tpl (E_variable y_var, y_ty) in
            let%bind yrec = aux y_var_expr y body in
            let%bind xrec = aux x_var_expr x yrec in
            return @@ E_let_tuple (expr, ([(x_var, x_ty); (y_var, y_ty)], xrec))
        in
        aux expr tree body
  in
  let%bind fun_type = compile_type fun_type in
  let%bind (input_type,output_type) = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_function fun_type in
  let loop_type = t_union (None, input_type) (None, output_type) in
  let%bind (body,binder) = map_lambda fun_name loop_type lambda.result in
  let binder = Location.map Var.todo_cast lambda.binder :: binder in
  let%bind binder = match binder with hd::[] -> ok @@ hd | _ -> fail @@ unsupported_recursive_function fun_name in
  let expr = Expression.make_tpl (E_variable binder, input_type) in
  let body = Expression.make (E_iterator (C_LOOP_LEFT, ((lambda.binder, input_type), body), expr)) output_type in
  ok @@ Expression.make (E_closure {binder;body}) fun_type

and compile_declaration module_env env (d:AST.declaration) : ((toplevel_statement * _ SMap.t) option , spilling_error) result =
  match d with
  | Declaration_type _ -> ok None
  | Declaration_constant { binder ; expr ; inline } ->
    let%bind expression = compile_expression ~module_env expr in
    let binder = Location.map Var.todo_cast binder in
    let tv = Combinators.Expression.get_type expression in
    let env' = Environment.add (binder, tv) env in
    ok @@ Some (((binder, inline, expression), environment_wrap env env'), module_env)
  | Declaration_module {module_binder; module_} ->
    let%bind record,module_env = compile_module_as_record module_binder module_env module_ in
    let binder = Location.wrap @@ Var.of_name module_binder in
    let tv = Combinators.Expression.get_type record in
    let env' = Environment.add (binder, tv) env in
    ok @@ Some (((binder, false, record), environment_wrap env env'), module_env)
  | Module_alias {alias; binders} ->
    let module_var, access = binders in
    let%bind module_ =
      trace_option (corner_case ~loc:__LOC__ "Mod_alias: This program shouldn't type")
      @@ SMap.find_opt module_var module_env  in
    let module_var = e_a_variable (Location.wrap (Var.of_name module_var)) module_ in
    let%bind module_expr = build_record_accessor module_var (List.map (fun l -> Label l) access) in
    let module_ = get_type_expression module_expr in
    let%bind module_expr = compile_expression ~module_env module_expr in
    let module_env  = SMap.add alias module_ module_env in
    let%bind module_type = compile_type module_ in
    let env' = Environment.add (Location.wrap @@ Var.of_name alias, module_type) env in
    ok @@ Some ((((Location.wrap @@ Var.of_name alias),true,module_expr),environment_wrap env env'),module_env)



and compile_module ?(module_env = SMap.empty) ((AST.Module_Fully_Typed lst) : AST.module_fully_typed) : (program * AST.type_expression SMap.t , spilling_error) result =
  let aux (prev:(toplevel_statement list * _ SMap.t * Environment.t , spilling_error) result) cur =
    let%bind (hds, module_env, env) = prev in
    match%bind compile_declaration module_env env cur with
    | Some (((_ , env') as cur', module_env)) -> ok (hds @ [ cur' ] , module_env, env'.post_environment)
    | None -> prev
  in
  let%bind (statements,map, _) = List.fold_left aux (ok ([], module_env,Environment.empty)) (temp_unwrap_loc_list lst) in
  ok (statements, map)

and compile_module_as_record module_name (module_env : _ SMap.t) (lst : AST.module_fully_typed) : (_ , spilling_error) result =
  let rec module_as_record module_env (AST.Module_Fully_Typed lst) : ((AST.expression * _),_) result =
    let aux (r,env) (cur : AST.declaration ) =
      match cur with
      | Declaration_constant { binder ; expr; inline=_ } ->
        let l = Var.to_name @@ Location.unwrap binder in
        ok @@ ((Label l,(expr,false))::r,env)
      | Declaration_type _ty -> ok @@ (r,env)
      | Declaration_module {module_binder; module_} ->
        let l = module_binder in
        let%bind r',_ = module_as_record env module_ in
        let env = SMap.add l (get_type_expression r') env in
        ok @@ ((Label l,(r',false))::r,env)
      | Module_alias {alias; binders} ->
        let l = alias in
        let module_var, access = binders in
        let%bind module_type =
          trace_option (corner_case ~loc:__LOC__ "Mod_alias: This program shouldn't type")
          @@ SMap.find_opt module_var env in
        let module_var = e_a_variable (Location.wrap (Var.of_name module_var)) module_type in
        let%bind module_expr = build_record_accessor module_var (List.map (fun l -> Label l) access) in
        let module_type = get_type_expression module_expr in
        let env = SMap.add l module_type env in
        let r' = module_expr in
        ok @@ ((Label l,(r',true))::r,env)

    in
    let%bind r,env = bind_fold_list aux ([],module_env) (temp_unwrap_loc_list lst) in
    if List.length r <> 0 then begin
      let build_record r =
        let aux (Label l, ((expr : AST.expression), _)) =
          let expr = {expr with expression_content = e_variable @@ Location.wrap @@ Var.of_name l} in
          (Label l, expr)
        in
        let record = ez_e_a_record @@ List.map aux (List.rev r) in
      (* prefix with let_in*)
        let aux record (Label l, (expr, inline)) =
          let binder = Location.wrap @@ Var.of_name l in
          AST.e_a_let_in binder expr record inline
        in
        let record = List.fold_left aux record r in
        record
      in
      let record = build_record r in
      ok @@ (record,env)
    end
    else
      ok @@ (e_a_unit,env)
  in
  let%bind record,env = module_as_record module_env lst in
  let module_env = SMap.add module_name (get_type_expression record) env in
  let%bind record = compile_expression ~module_env record in
  ok @@ (record, module_env)
