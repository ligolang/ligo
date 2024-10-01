open Core
open Grace
open Ligo_prim
module I = Mini_c
module O = Lltz_ir
module Ligo_string = Simple_utils.Ligo_string
module Location = Simple_utils.Location
module Lltz_codegen = Lltz_codegen
module Michelson = Michelson

let compile_location (loc : Location.t) : Range.t =
  match loc with
    | File reg -> 
      (try
        Range.of_lex ~source:(`File reg#file) reg#byte_pos
      with _ -> Range.initial (`String { content = ""; name = Some (reg#file) }))
    | Virtual name -> Range.initial (`String { content = ""; name = Some name })

let dummy_loc = Location.generated
let dummy_range = compile_location dummy_loc

let compile_annot (annot : string option) : O.Row.label option =
  match annot with
  | None -> None
  | Some annot -> Some (Label annot)

let rec compile_type_expression (type_ : I.type_expression) : O.Type.t =
  let return (desc : O.Type.desc) : O.Type.t =
    { desc; range = compile_location type_.location }
  in
  match type_.type_content with
  | T_option type_ -> return @@ Option (compile_type_expression type_)
  | T_list type_ -> return @@ List (compile_type_expression type_)
  | T_map (key_type, value_type) ->
    return @@ Map (compile_type_expression key_type, compile_type_expression value_type)
  | T_big_map (key_type, value_type) ->
    return
    @@ Big_map (compile_type_expression key_type, compile_type_expression value_type)
  | T_set elt_type -> return @@ Set (compile_type_expression elt_type)
  | T_contract arg_type -> return @@ Contract (compile_type_expression arg_type)
  | T_ticket type_ -> return @@ Ticket (compile_type_expression type_)
  | T_sapling_state memo -> return @@ Sapling_state { memo = (Z.to_int memo) }
  | T_sapling_transaction memo -> return @@ Sapling_transaction { memo = (Z.to_int memo) }
  | T_function (arg_type, ret_type) ->
    return @@ Function (compile_type_expression arg_type, compile_type_expression ret_type)
  | T_tuple annot_types -> return @@ Tuple (compile_row annot_types)
  | T_or (left, right) -> return @@ Or (compile_row [ left; right ])
  | T_base TB_unit -> return Unit
  | T_base TB_bool -> return Bool
  | T_base TB_string -> return String
  | T_base TB_bytes -> return Bytes
  | T_base TB_nat -> return Nat
  | T_base TB_int -> return Int
  | T_base TB_mutez -> return Mutez
  | T_base TB_operation -> return Operation
  | T_base TB_address -> return Address
  | T_base TB_key -> return Keys
  | T_base TB_key_hash -> return Key_hash
  | T_base TB_chain_id -> return Chain_id
  | T_base TB_signature -> return Signature
  | T_base TB_timestamp -> return Timestamp
  | T_base TB_bls12_381_g1 -> return Bls12_381_g1
  | T_base TB_bls12_381_g2 -> return Bls12_381_g2
  | T_base TB_bls12_381_fr -> return Bls12_381_fr
  | T_base TB_never -> return Never
  | T_base TB_tx_rollup_l2_address -> return Tx_rollup_l2_address
  | T_base (TB_type_int _) -> return Int
  | T_base TB_chest -> return Chest
  | T_base TB_chest_key -> return Chest_key
  (* dead baker account support *)
  | T_base TB_baker_hash -> assert false
  | T_base TB_pvss_key -> assert false
  | T_base TB_baker_operation -> assert false

and compile_annotated_type_expression (annot_type : I.type_expression I.annotated) =
  let annot, type_ = annot_type in
  compile_annot annot, compile_type_expression type_

and compile_row annot_types =
  let open O.Row in
  let labelled_types = List.map annot_types ~f:compile_annotated_type_expression in
  Node (List.map labelled_types ~f:(fun (label, type_) -> Leaf (label, type_)))

let compile_literal (lit : Literal_value.t) : O.Expr.constant =
  match lit with
  | Literal_unit -> Unit
  | Literal_int z -> Int z
  | Literal_nat z -> Nat z
  | Literal_timestamp z -> 
    (match Ptime.of_span (Ptime.Span.of_int_s (Z.to_int z)) with
    | Some time -> Timestamp (Ptime.to_rfc3339 time)
    | None -> assert false)
  | Literal_mutez mutez -> Mutez mutez
  | Literal_string s -> String (Ligo_string.extract s)
  | Literal_bytes b -> Bytes (Bytes.to_string b)
  | Literal_address addr -> Address addr
  | Literal_signature sig_ -> Signature sig_
  | Literal_key key -> Key key
  | Literal_key_hash key_hash -> Key_hash key_hash
  | Literal_chain_id chain_id -> Chain_id chain_id
  | Literal_bls12_381_g1 b -> Bls12_381_g1 (Bytes.to_string b)
  | Literal_bls12_381_g2 b -> Bls12_381_g2 (Bytes.to_string b)
  | Literal_bls12_381_fr b -> Bls12_381_fr (Bytes.to_string b)
  | Literal_chest chest -> Bytes (Bytes.to_string chest)
  | Literal_chest_key chest_key -> Bytes (Bytes.to_string chest_key)
  (* User cannot craft this literal, solely used for the interpreter *)
  | Literal_operation _ -> assert false

let compile_constant (const : Constant.constant') (args: O.Expr.t list) (return_ty: O.Type.t) (range: Range.t): O.Expr.desc = 
  let mk_prim ?(args=args) desc = O.Expr.Prim(desc, args) in
  match const with
  | C_UNIT -> mk_prim Unit
  | C_NIL -> (match return_ty with
  | { desc = List ty; _ } -> mk_prim (Nil (ty))
  | _ -> assert false) 
  | C_SOME -> mk_prim Some
  | C_NONE -> (
    match return_ty with
    | { desc = Option ty; _ } -> mk_prim (None ty)
    | _ -> assert false)
  | C_UPDATE -> assert false
  | C_ITER -> assert false
  | C_LOOP_LEFT -> assert false
  | C_LOOP_CONTINUE -> (
    match args, return_ty.desc with
    | a::tl, Or (O.Row.Node [O.Row.Leaf(None, ty1);O.Row.Leaf(None, ty2)]) -> (O.Dsl.left ~range (None,None,ty2) a).desc
    | _ -> assert false
  )
  | C_LOOP_STOP -> (
    match args, return_ty.desc with
    | a::tl, Or (O.Row.Node [O.Row.Leaf(None, ty1);O.Row.Leaf(None, ty2)]) -> (O.Dsl.right ~range (None,None,ty1) a).desc
    | _ -> assert false
  )
  | C_FOLD -> assert false
  | C_FOLD_LEFT -> assert false
  | C_FOLD_RIGHT -> assert false
  | C_ABS -> mk_prim Abs
  | C_INT -> mk_prim Int
  | C_NEG -> mk_prim Neg
  | C_ADD -> mk_prim Add
  | C_SUB -> (
    match args with
    | a::b::tl -> (O.Dsl.sub ~range a b).desc
    | _ -> assert false)
  | C_MUL -> mk_prim Mul
  | C_DIV -> (
    match args with
    | a::b::tl -> (O.Dsl.div_ ~range a b).desc
    | _ -> assert false)
  | C_MOD -> (match args with
    | a::b::tl -> (O.Dsl.mod_ ~range a b).desc
    | _ -> assert false)
  | C_NOT -> mk_prim Not
  | C_AND -> mk_prim And
  | C_OR -> mk_prim Or
  | C_XOR -> mk_prim Xor
  | C_LAND -> mk_prim And
  | C_LOR -> mk_prim Or
  | C_LXOR -> mk_prim Xor
  | C_LSL -> mk_prim Lsl
  | C_LSR -> mk_prim Lsr
  | C_EQ -> (
    match args with
    | a::b::tl -> mk_prim ~args:((O.Dsl.compare_ ~range a b)::tl) Eq
    | _ -> assert false)
  | C_NEQ -> (
    match args with
    | a::b::tl -> mk_prim ~args:((O.Dsl.compare_ ~range a b)::tl) Neq
    | _ -> assert false)
  | C_LT -> (
    match args with
    | a::b::tl -> mk_prim ~args:((O.Dsl.compare_ ~range a b)::tl) Lt
    | _ -> assert false)
  | C_GT -> (
    match args with
    | a::b::tl -> mk_prim ~args:((O.Dsl.compare_ ~range a b)::tl) Gt
    | _ -> assert false)
  | C_LE -> (
    match args with
    | a::b::tl -> mk_prim ~args:((O.Dsl.compare_ ~range a b)::tl) Le
    | _ -> assert false)
  | C_GE -> (
    match args with
    | a::b::tl -> mk_prim ~args:((O.Dsl.compare_ ~range a b)::tl) Ge
    | _ -> assert false)
  | C_CONCAT -> mk_prim Concat2
  | C_CONCATS -> mk_prim Concat1
  | C_CONS -> mk_prim Cons
  | C_SIZE -> mk_prim Size
  | C_SLICE -> (
    match args with
    | offset::length::seq::tl -> (
      O.Dsl.if_none (O.Dsl.slice ~range offset ~length ~seq)
        ~some:(let var_name = O.Dsl.gen_name () in
          O.Dsl.annon_function var_name seq.type_ ~body:(O.Dsl.variable (Var var_name) seq.type_))
        ~none:(O.Dsl.failwith ~range (O.Dsl.string ~range "Slice out of bounds"))
      ).desc
    | _ -> assert false)
  | C_PAIR -> mk_prim (Pair (None, None))
  | C_CAR -> mk_prim Car
  | C_CDR -> mk_prim Cdr
  | C_TRUE -> O.Expr.Const(Bool true)
  | C_FALSE -> O.Expr.Const(Bool false)
  | C_LEFT -> (
    match return_ty with
    | { desc = Or (Node [Leaf(_,_); Leaf(_,ty2)]); _ } -> mk_prim (Left (None, None, ty2))
    | _ -> assert false)
  | C_RIGHT -> (
    match return_ty with
    | { desc = Or (Node [Leaf(_,ty1); Leaf(_,_)]); _ } -> mk_prim (Right (None, None, ty1))
    | _ -> assert false)
  | C_SET_EMPTY -> (
    match return_ty with
    | { desc = Set ty; _ } -> mk_prim (Empty_set (ty))
    | _ -> assert false)
  | C_SET_LITERAL -> assert false
  | C_SET_ADD -> 
    (match args with
    | arg_hd::arg_tl -> mk_prim ~args:(arg_hd::(O.Dsl.bool ~range true)::arg_tl) Update
    | _ -> assert false)
  | C_SET_REMOVE -> 
    (match args with
    | arg_hd::arg_tl -> mk_prim ~args:(arg_hd::(O.Dsl.bool ~range false)::arg_tl) Update
    | _ -> assert false)
  | C_SET_ITER -> assert false
  | C_SET_FOLD -> assert false
  | C_SET_FOLD_DESC -> assert false
  | C_SET_MEM -> mk_prim Mem
  | C_SET_UPDATE -> 
    (match args with
    | elem::flag::coll -> mk_prim ~args:(elem::flag::coll) Update
    | _ -> assert false)
  | C_SET_SIZE -> mk_prim Size
  | C_LIST_EMPTY -> (
    match return_ty with
    | { desc = List ty; _ } -> mk_prim (Nil (ty))
    | _ -> assert false) 
  | C_LIST_LITERAL -> assert false
  | C_LIST_ITER -> assert false
  | C_LIST_MAP -> assert false
  | C_LIST_FOLD -> assert false
  | C_LIST_FOLD_LEFT -> assert false
  | C_LIST_FOLD_RIGHT -> assert false
  | C_LIST_SIZE -> mk_prim Size
  | C_MAP -> 
    (match return_ty with
    | { desc = Map (key_ty, value_ty); _ } -> mk_prim (Empty_map (key_ty, value_ty))
    | _ -> assert false)
  | C_MAP_EMPTY -> (
    match return_ty with
    | { desc = Map (key_ty, value_ty); _ } -> mk_prim (Empty_map (key_ty, value_ty))
    | _ -> assert false)
  | C_MAP_LITERAL -> assert false
  | C_MAP_GET -> mk_prim Get
  | C_MAP_GET_FORCE -> assert false
  | C_MAP_ADD -> (
    match args with
    | key::value::arg_tl -> mk_prim ~args:(key::(O.Dsl.some ~range value)::arg_tl) Update
    | _ -> assert false)
  | C_MAP_REMOVE -> (
    match args, return_ty with
    | key::arg_tl, { desc = Map (key_ty, value_ty); _ } -> mk_prim ~args:(key::(O.Dsl.none ~range value_ty)::arg_tl) Update
    | _ -> assert false)
  | C_MAP_UPDATE -> mk_prim Update
  | C_MAP_ITER -> assert false
  | C_MAP_MAP -> assert false
  | C_MAP_FOLD -> assert false
  | C_MAP_FIND -> (
    match args with
    | key::coll::tl -> (
      O.Dsl.if_none (O.Dsl.get ~range key coll)
        ~some:(let var_name = O.Dsl.gen_name () in
          (match coll.type_ with
          | { desc = Map (key_ty, value_ty); _ } -> O.Dsl.annon_function var_name value_ty ~body:(O.Dsl.variable (Var var_name) value_ty)
          | _ -> assert false)
          )
        ~none:(O.Dsl.failwith ~range (O.Dsl.string ~range "Key not found"))
      ).desc
    | _ -> assert false)
  | C_MAP_FIND_OPT -> mk_prim Get 
  | C_MAP_GET_AND_UPDATE -> mk_prim Get_and_update
  | C_MAP_SIZE -> mk_prim Size
  | C_MAP_MEM -> mk_prim Mem
  | C_BIG_MAP -> (
    match return_ty with
    | { desc = Big_map (key_ty, value_ty); _ } -> mk_prim (Empty_bigmap (key_ty, value_ty))
    | _ -> assert false)
  | C_BIG_MAP_EMPTY -> (
    match return_ty with
    | { desc = Big_map (key_ty, value_ty); _ } -> mk_prim (Empty_bigmap (key_ty, value_ty))
    | _ -> assert false)
  | C_BIG_MAP_LITERAL -> assert false
  | C_BIG_MAP_GET_AND_UPDATE -> mk_prim Get_and_update
  | C_BIG_SET_LITERAL -> assert false
  | C_CREATE_CONTRACT -> assert false
  | C_CAST_DYNAMIC_ENTRYPOINT ->
    (* removed in checking *)
    assert false
  | C_CHECK_SELF | C_CHECK_EMIT_EVENT | C_CHECK_ENTRYPOINT | C_CHECK_CALL_VIEW_LITSTR ->
    (* removed in AST aggregated *)
    assert false
  | C_TEST_ADDRESS
  | C_TEST_SIZE
  | C_TEST_ORIGINATE
  | C_TEST_GET_STORAGE
  | C_TEST_GET_BALANCE
  | C_TEST_SET_SOURCE
  | C_TEST_SET_BAKER
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN
  | C_TEST_GET_NTH_BS
  | C_TEST_PRINT
  | C_TEST_TO_STRING
  | C_TEST_UNESCAPE_STRING
  | C_TEST_STATE_RESET
  | C_TEST_BOOTSTRAP_CONTRACT
  | C_TEST_NTH_BOOTSTRAP_CONTRACT
  | C_TEST_LAST_ORIGINATIONS
  | C_TEST_MUTATE_CONTRACT
  | C_TEST_MUTATE_VALUE
  | C_TEST_SAVE_MUTATION
  | C_TEST_RUN
  | C_TEST_COMPILE_CONTRACT
  | C_TEST_DECOMPILE
  | C_TEST_TO_CONTRACT
  | C_TEST_TO_ENTRYPOINT
  | C_TEST_COMPILE_CONTRACT_FROM_FILE
  | C_TEST_COMPILE_AST_CONTRACT
  | C_TEST_TO_TYPED_ADDRESS
  | C_TEST_TO_ADDRESS
  | C_TEST_CAST_ADDRESS
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS
  | C_TEST_SET_BIG_MAP
  | C_TEST_RANDOM
  | C_TEST_GENERATOR_EVAL
  | C_TEST_ADD_ACCOUNT
  | C_TEST_NEW_ACCOUNT
  | C_TEST_BAKER_ACCOUNT
  | C_TEST_REGISTER_DELEGATE
  | C_TEST_STAKE
  | C_TEST_BAKE_UNTIL_N_CYCLE_END
  | C_TEST_GET_VOTING_POWER
  | C_TEST_GET_TOTAL_VOTING_POWER
  | C_TEST_REGISTER_CONSTANT
  | C_TEST_CONSTANT_TO_MICHELSON
  | C_TEST_REGISTER_FILE_CONSTANTS
  | C_TEST_PUSH_CONTEXT
  | C_TEST_POP_CONTEXT
  | C_TEST_DROP_CONTEXT
  | C_TEST_FAILWITH
  | C_TEST_READ_CONTRACT_FROM_FILE
  | C_TEST_SIGN
  | C_TEST_GET_ENTRYPOINT
  | C_TEST_INT64_OF_INT
  | C_TEST_INT64_TO_INT
  | C_TEST_LAST_EVENTS
  | C_TEST_TRY_WITH
  | C_TEST_SET_PRINT_VALUES
  | C_TEST_NIL_VIEWS
  | C_TEST_CONS_VIEWS
  | C_TEST_COMPARE
  | C_TEST_CREATE_CHEST
  | C_TEST_CREATE_CHEST_KEY
  | C_TEST_VERIFY_CHEST ->
    (* only interpreter *)
    assert false
  | C_GLOBAL_CONSTANT ->
    (* TODO: removed in ?? *) 
    assert false 
  | C_POLYMORPHIC_ADD ->
    (* removed in checking *)
    assert false
  | C_POLYMORPHIC_SUB ->
    (* removed in checking *)
    assert false
  | C_SUB_MUTEZ -> mk_prim Sub_mutez
  | C_OPTION_MAP -> assert false
  | C_OPT_OUT_ENTRY ->
    (* removed in checking *)
    assert false

let compile_var (var : Value_var.t) : O.Expr.var =
  Var (Format.asprintf "%a" Value_var.pp var)

let compile_mut_var (var : Value_var.t) : O.Expr.mut_var =
  Mut_var (Format.asprintf "%a" Value_var.pp var)

let compile_binder ((var, _type) : I.binder) : O.Expr.var * O.Type.t =
  Var (Format.asprintf "%a" Value_var.pp var), compile_type_expression _type

let compile_mut_binder ((var, _type) : I.binder) : O.Expr.mut_var * O.Type.t =
  Mut_var (Format.asprintf "%a" Value_var.pp var), compile_type_expression _type

let compile_micheline_seq nodes =
  let open Tezos_micheline in
  let nodes = List.map nodes ~f:(Micheline.map_node compile_location Fn.id) in
  let range =
    match nodes with
    | [] -> dummy_range
    | hd :: tl ->
      let start = Micheline.location hd in
      let stop = List.last tl |> Option.value_map ~f:Micheline.location ~default:start in
      Range.merge start stop
  in
  Micheline.Seq (range, nodes)

let rec compile_expression (expr : I.expression) : O.Expr.t =
  let return_ty = compile_type_expression expr.type_expression in
  let range = compile_location expr.location in
  let return (desc : O.Expr.desc) : O.Expr.t =
    { desc; range = range; type_ = return_ty }
  in
  match expr.content with
  | E_literal lit -> return @@ Const (compile_literal lit)
  | E_closure func ->
    let var, _return_type, body = compile_function func in
    let var_ty = (match return_ty with
    | { desc = O.Type.Function (var_ty, _return_type); _ } -> var_ty
    | _ -> assert false) in
    return @@ O.Expr.Lambda { lam_var = (var, var_ty); body }
  | E_rec { func; rec_binder } -> 
    let rec_var = compile_var rec_binder in
    let var, _return_type, body = compile_function func in
    let var_ty = (match return_ty with
    | { desc = O.Type.Function (var_ty, _return_type); _ } -> var_ty
    | _ -> assert false) in
    return @@ Lambda_rec { mu_var = (rec_var, return_ty); lambda ={lam_var = (var, var_ty); body} }
  | E_constant { cons_name; arguments } ->
    let args = List.map arguments ~f:compile_expression in
    return @@ (compile_constant cons_name args return_ty range)
  | E_application (abs, arg) ->
    let abs = compile_expression abs in
    let arg = compile_expression arg in
    return @@ App { abs; arg }
  | E_variable var -> return @@ (Variable (compile_var var))
  | E_let_in (rhs, _inline, (binder, in_)) ->
    let (let_var, _) = compile_binder binder in
    let rhs = compile_expression rhs in
    let in_ = compile_expression in_ in
    return @@ Let_in { let_var; rhs; in_ }
  | E_let_mut_in (rhs, (binder, in_)) ->
    let let_var, _ = compile_mut_binder binder in
    let rhs = compile_expression rhs in
    let in_ = compile_expression in_ in
    return @@ Let_mut_in { let_var; rhs; in_ }
  | E_deref var -> return @@ Deref (compile_mut_var var)
  | E_assign (var, value) ->
    let var = compile_mut_var var in
    let value = compile_expression value in
    return @@ Assign (var, value)
  | E_if_bool (condition, if_true, if_false) ->
    let condition = compile_expression condition in
    let if_true = compile_expression if_true in
    let if_false = compile_expression if_false in
    return @@ If_bool { condition; if_true; if_false }
  | E_if_cons (subject, if_nil, ((hd_binder, tl_binder), if_cons)) -> 
    let subject = compile_expression subject in
    let if_nil = compile_expression if_nil in
    let hd_binder = compile_binder hd_binder in
    let tl_binder = compile_binder tl_binder in
    let if_cons = compile_expression if_cons in
    return @@ If_cons { subject; if_empty = if_nil; if_nonempty = {lam_var1 = hd_binder; lam_var2 = tl_binder; body = if_cons} }
  | E_if_none (subject, if_none, (binder, if_some)) ->
    let subject = compile_expression subject in
    let if_none = compile_expression if_none in
    let binder = compile_binder binder in
    let if_some = compile_expression if_some in
    return @@ If_none { subject; if_none; if_some = {lam_var = binder; body = if_some} }
  | E_if_left (subject, (left_binder, if_left), (right_binder, if_right)) ->
    let subject = compile_expression subject in
    let left_binder = compile_binder left_binder in
    let if_left = compile_expression if_left in
    let right_binder = compile_binder right_binder in
    let if_right = compile_expression if_right in
    return
    @@ If_left
         { subject; if_left = {lam_var = left_binder; body = if_left}; if_right = {lam_var = right_binder; body=if_right} }
  | E_while (cond, body) ->
    let cond = compile_expression cond in
    let body = compile_expression body in
    return @@ While { cond; body }
  | E_for (start, stop, step, (index, body)) ->
    let start = compile_expression start in
    let stop = compile_expression stop in
    let step = compile_expression step in
    let index, index_ty = compile_mut_binder index in
    let body = compile_expression body in
    return @@ For { init = start; 
      cond = O.Dsl.le ~range (O.Dsl.deref ~range index index_ty) (stop); 
      update = O.Dsl.assign index (O.Dsl.add ~range (O.Dsl.deref ~range index index_ty) step); 
      index; body }
  | E_for_each (collection, _type, (binders, body)) ->
    let collection = compile_expression collection in
    let var, body = compile_binders binders ~in_:(compile_expression body) in
    return @@ For_each { collection; body = {lam_var = var; body} }
  | E_tuple elts ->
    let elts = List.map elts ~f:compile_expression in
    let row = O.Row.(Node (List.map elts ~f:(fun elt -> Leaf (None, elt)))) in
    return @@ Tuple row
  | E_proj (tuple, index, _tuple_size) ->
    let tuple = compile_expression tuple in
    return @@ Proj (tuple, O.Row.Path.Here [ index ])
  | E_update (tuple, index, update, _tuple_size) ->
    let tuple = compile_expression tuple in
    let update = compile_expression update in
    return @@ Update { tuple; component = O.Row.Path.Here [ index ]; update }
  | E_let_tuple (rhs, (binders, in_)) ->
    let components = List.map (List.map binders ~f:compile_binder) ~f:fst in
    let rhs = compile_expression rhs in
    let in_ = compile_expression in_ in
    return @@ Let_tuple_in { components; rhs; in_ }
  | E_iterator (iter_const, (binder, body), collection) ->
    let collection = compile_expression collection in
    let binder = compile_binder binder in
    let body = compile_expression body in
    (match iter_const with
    | Constant.C_ITER -> return @@ For_each { collection; body = {lam_var = binder; body} }
    | Constant.C_MAP -> return @@ Map { collection; map = {lam_var = binder; body} }
    | Constant.C_LOOP_LEFT ->
      return @@ While_left { cond = collection; body = {lam_var = binder; body} }
    | _ -> assert false)
  | E_fold ((binder, body), collection, init) ->
    let collection = compile_expression collection in
    let init = compile_expression init in
    let binder = compile_binder binder in
    let body = compile_expression body in
    return @@ Fold_left { collection; init; fold = {lam_var=binder; body} }
  | E_fold_right ((binder, body), (collection, _elt_type), init) ->
    let collection = compile_expression collection in
    let init = compile_expression init in
    let binder = compile_binder binder in
    let body = compile_expression body in
    return @@ Fold_right { collection; init; fold = {lam_var=binder; body} }
  | I.E_raw_michelson nodes ->
    let micheline = compile_micheline_seq nodes in
    let var = compile_var (Value_var.fresh ~loc:dummy_loc ()) in
    (match return_ty with
    | { desc = O.Type.Function (var_ty, body_ty); _ } ->
      return
      @@ Lambda
           { lam_var = (var, var_ty)
           ; body = {desc = Raw_michelson { michelson = micheline; args = [ O.Dsl.variable ~range var var_ty ]}; range = range; type_= body_ty}
           }
    | _ -> assert false)
  | E_inline_michelson (code, args') ->
    let micheline, args = compile_inline_michelson (code, args') in
    return @@ Raw_michelson { michelson = micheline; args }

  | I.E_global_constant (hash, args) ->
    assert false
    (*TODO: let args = List.map args ~f:compile_expression in
    return @@ Global_constant { hash; args }

    (* { ...  
        PUSH args onto stack
       ; (constant <hash>)

       }  *)*)

  | I.E_create_contract
      ( parameter_type
      , storage_type
      , (binder, code)
      , [ delegate; initial_balance; initial_storage ] ) ->
    let parameter = compile_type_expression parameter_type in
    let storage = compile_type_expression storage_type in
    let binder = compile_binder binder in
    let code = compile_expression code in
    let delegate = compile_expression delegate in
    let initial_balance = compile_expression initial_balance in
    let initial_storage = compile_expression initial_storage in
    return
    @@ Create_contract
         { 
         storage
         ; code = { lam_var = binder; body = code }
         ; delegate
         ; initial_balance
         ; initial_storage
         }
  | E_create_contract _ -> assert false

and compile_contract (binder:Value_var.t) (input_ty: I.type_expression) (body:I.expression) =
  let binder = compile_var binder in
  let input_ty = compile_type_expression input_ty in
  let body = compile_expression body in
  binder, input_ty, body

and compile_function ({ binder; body } : I.anon_function) =
  let ret_ty = compile_type_expression body.type_expression in
  let body = compile_expression body in
  let lam_var = compile_var binder in
  lam_var, ret_ty, body

and compile_tuple_index (index : int) : O.Row.Path.t = O.Row.Path.Here [ index ]

and compile_binders binders ~(in_ : O.Expr.t) : (O.Expr.var * O.Type.t) * O.Expr.t =
  let create_expr (desc : O.Expr.desc) : O.Expr.t = { desc = desc; range = in_.range; type_ = in_.type_  } in
  match binders with
  | [] -> assert false
  | [ binder ] -> compile_binder binder, in_
  | binders ->
    let var = compile_var (Value_var.fresh ~loc:dummy_loc ()) in
    let binders = List.map binders ~f:compile_binder in
    ( (var, {desc = Tuple (O.Row.Node (List.map binders ~f:(fun (var, ty) -> O.Row.Leaf (None, ty)))); range = dummy_range })
    , create_expr
      @@ Let_tuple_in { components = List.map binders ~f:fst; rhs = create_expr @@ Variable var; in_ } )

and compile_inline_michelson (code, args') =
  let args_compiled = List.map args' ~f:(
      fun e -> (
        compile_expression e
    )) in
    let args_ty = List.map args' ~f:(
      fun e -> (
        Mini_c.PP.type_expression Format.str_formatter e.type_expression;
        compile_type_expression e.type_expression;
    )) in
    let wipe_locations l e =
      Tezos_micheline.Micheline.(inject_locations (fun _ -> l) (strip_locations e))
    in
    let code = (List.map ~f:(wipe_locations (())) code) in
    let used = ref [] in
    let replace m =
      let open Tezos_micheline.Micheline in
      match m with
      | Prim (_, s, [], [ id ])
        when String.equal "typeopt" s && String.is_prefix ~prefix:"$" id ->
        let id = String.chop_prefix_exn ~prefix:"$" id in
        let id = Int.of_string id in
        used := id :: !used;
        (match List.nth args_ty id with
        | Some (prim) ->
          (match (Lltz_codegen.convert_type prim) with
          | (Prim (_, Michelson.Ast.Prim.T Michelson.Ast.Prim.Type.Option, [ t ], _))  
            -> Tezos_micheline.Micheline.map_node (fun _ -> ()) (fun prim -> Michelson.Ast.Prim.to_string prim) t
          | _ -> raise_s [%message "could not resolve (typeopt $)" (id : int)])
        | _ -> raise_s [%message "could not resolve (typeopt $)" (id : int)])
      | Prim (_, s, [], [ id ])
        when String.equal "type" s && String.is_prefix ~prefix:"$" id ->
        let id = String.chop_prefix_exn ~prefix:"$" id in
        let id = Int.of_string id in
        used := id :: !used;
        (match List.nth args_ty id with
        | None -> raise_s [%message "could not resolve (type $)" (id : int)]
        | Some (t) -> 
          Tezos_micheline.Micheline.map_node (fun _ -> ()) (fun prim -> Michelson.Ast.Prim.to_string prim) (Lltz_codegen.convert_type t))
      | Prim (_, s, [], [ id ])
        when String.equal "litstr" s && String.is_prefix ~prefix:"$" id ->
        let id = String.chop_prefix_exn ~prefix:"$" id in
        let id = Int.of_string id in
        used := id :: !used;
        (match List.nth args_compiled id with
        | Some ({desc = Const(O.Expr.String (s)); _}) -> String ((), s)
        | _ -> raise_s [%message "could not resolve (litstr $)" (id : int)])
      | Prim (_, s, [], [ id ])
        when String.equal "codestr" s && String.is_prefix ~prefix:"$" id ->
        let id = String.chop_prefix_exn ~prefix:"$" id in
        let id = Int.of_string id in
        used := id :: !used;
        (match List.nth args_compiled id with
        | Some ({desc = Const(O.Expr.String (s)); _}) ->
          let open Tezos_micheline in
          let code = s in
          let code, errs = Micheline_parser.tokenize code in
          (match errs with
          | _ :: _ ->
            raise_s [%message "could not parse raw michelson"]
          | [] ->
            let code, errs = Micheline_parser.parse_expression ~check:false code in
            (match errs with
            | _ :: _ ->
              raise_s [%message "could not parse raw michelson"]
            | [] ->
              let code = Micheline.strip_locations code in
              (* hmm *)
              let code = Micheline.inject_locations (fun _ -> Location.generated) code in
              map_node (fun _ -> ()) (fun x -> x) code))
        | _ -> raise_s [%message "could not resolve (codestr $)" (id : int)])
      | Prim (a, b, c, d) ->
        let open Tezos_micheline.Micheline in
        let f arg (c, d) =
          match arg with
          | Prim (_, s, [], [ id ])
            when String.equal "annot" s && String.is_prefix ~prefix:"$" id ->
            let id = String.chop_prefix_exn ~prefix:"$" id in
            let id = Int.of_string id in
            used := id :: !used;
            let annot =
              match List.nth args_compiled id with
              | Some ({desc = Const(O.Expr.String (s)); _}) -> s
              | _ -> raise_s [%message "could not resolve (annot $)" (id : int)]
            in
            c, annot :: d
          | m -> m :: c, d
        in
        let c, d = List.fold_right ~f ~init:([], d) c in
        Prim (a, b, c, d)
      | m -> m
    in
    let code = List.map ~f:(Tezos_utils.Michelson.map replace) code in
    let args' =
      List.filter_mapi
        ~f:(fun i v ->
          if not (List.mem !used i ~equal:Stdlib.( = )) then Some v else None)
        args'
    in
    let code = (List.map ~f:(wipe_locations (dummy_loc)) code) in
    let micheline = compile_micheline_seq code in
    let args = List.map args' ~f:compile_expression in
    micheline, args