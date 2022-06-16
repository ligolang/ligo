module Errors = Errors
open Errors
open Mini_c
open Simple_utils.Trace

let eta_expand : expression -> type_expression -> type_expression -> anon_function =
  fun e in_ty out_ty ->
    let binder = ValueVar.fresh () in
    let var = e_var binder in_ty in
    let app = e_application e out_ty var in
    { binder = binder ; body = app }
let get_t_function ~raise e =
  trace_option ~raise not_a_function @@ Mini_c.get_t_function e
let get_t_pair ~raise e =
  trace_option ~raise not_a_pair @@ Mini_c.get_t_pair e

let get_function_or_eta_expand ~raise e =
  let in_ty, out_ty = match e.type_expression.type_content with
    | T_function t -> t
    | _ -> raise.error (corner_case "contract do not have the type of a function")
  in
  match e.content with
  | E_closure f -> f
  | _ ->
    eta_expand e in_ty out_ty

(* TODO hack to specialize map_expression to identity monad *)
let map_expression = Helpers.map_expression

(* Conservative purity test: ok to treat pure things as impure, must
   not treat impure things as pure. *)

(* true if the name names a pure constant -- i.e. if uses will be pure
   assuming arguments are pure *)
let is_pure_constant : constant' -> bool =
  function
  | C_UNIT
  | C_CAR | C_CDR | C_PAIR
  | C_NIL | C_CONS
  | C_NEG | C_OR | C_AND | C_XOR | C_NOT
  | C_EQ  | C_NEQ | C_LT | C_LE | C_GT | C_GE
  | C_NONE | C_SOME
  | C_LEFT | C_RIGHT
  | C_TRUE | C_FALSE
  | C_UPDATE | C_MAP_FIND_OPT | C_MAP_ADD | C_MAP_UPDATE
  | C_ADDRESS
  | C_CONCAT
  | C_SET_MEM | C_SET_ADD | C_SET_REMOVE | C_SET_UPDATE
  | C_LOOP_CONTINUE | C_LOOP_STOP
  | C_SUB_MUTEZ
  | C_BYTES_UNPACK
  | C_SET_EMPTY | C_SET_LITERAL
  | C_LIST_EMPTY | C_LIST_LITERAL
  | C_MAP_EMPTY | C_MAP_LITERAL
  | C_MAP_GET | C_MAP_REMOVE
  | C_MAP_GET_AND_UPDATE | C_BIG_MAP_GET_AND_UPDATE
  | C_SAPLING_EMPTY_STATE
  | C_SAPLING_VERIFY_UPDATE
  | C_OPEN_CHEST
  | C_GLOBAL_CONSTANT (* pure because restricted to PUSH *)
    -> true
  (* unfortunately impure: *)
  | C_ADD | C_SUB |C_MUL|C_DIV|C_MOD | C_LSL | C_LSR
  | C_POLYMORPHIC_ADD | C_POLYMORPHIC_SUB
  (* impure: *)
  | C_UNOPT
  | C_UNOPT_WITH_ERROR
  | C_OPTION_MAP
  | C_ASSERT_INFERRED
  | C_MAP_FIND
  | C_CALL
  | C_ITER
  | C_LOOP_LEFT
  | C_FOLD
  | C_FOLD_LEFT
  | C_FOLD_RIGHT
  | C_SET_ITER
  | C_SET_FOLD
  | C_SET_FOLD_DESC
  | C_LIST_ITER
  | C_LIST_MAP
  | C_LIST_FOLD
  | C_LIST_FOLD_LEFT
  | C_LIST_FOLD_RIGHT
  | C_MAP_GET_FORCE
  | C_MAP_ITER
  | C_MAP_MAP
  | C_MAP_FOLD
  | C_SET_DELEGATE
  | C_CREATE_CONTRACT
  (* TODO? *)
  | C_MAP
  | C_BIG_MAP
  | C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL
  | C_CONTRACT
  | C_CONTRACT_WITH_ERROR
  | C_CONTRACT_OPT
  | C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT
  | C_SELF
  | C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT
  | C_VIEW
  (* Test - ligo interpreter, should never end up here *)
  | C_TEST_SIZE
  | C_TEST_ORIGINATE
  | C_TEST_GET_STORAGE_OF_ADDRESS
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
  | C_TEST_MUTATE_VALUE
  | C_TEST_MUTATION_TEST
  | C_TEST_MUTATION_TEST_ALL
  | C_TEST_SAVE_MUTATION
  | C_TEST_RUN
  | C_TEST_COMPILE_CONTRACT
  | C_TEST_DECOMPILE
  | C_TEST_TO_CONTRACT
  | C_TEST_TO_ENTRYPOINT
  | C_TEST_TO_TYPED_ADDRESS
  | C_TEST_RANDOM
  | C_TEST_GENERATOR_EVAL
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS
  | C_TEST_COMPILE_CONTRACT_FROM_FILE
  | C_TEST_SET_BIG_MAP
  | C_TEST_CAST_ADDRESS
  | C_TEST_CREATE_CHEST
  | C_TEST_CREATE_CHEST_KEY
  | C_TEST_ADD_ACCOUNT
  | C_TEST_NEW_ACCOUNT
  | C_TEST_BAKER_ACCOUNT
  | C_TEST_REGISTER_DELEGATE
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
    -> false

let rec is_pure : expression -> bool = fun e ->
  match e.content with
  | E_literal _
  | E_closure _
  | E_variable _
  | E_raw_michelson _
    -> true

  | E_if_bool (cond, bt, bf)
  | E_if_none (cond, bt, (_, bf))
  | E_if_cons (cond, bt, (_, bf))
  | E_if_left (cond, (_, bt), (_, bf))
    -> List.for_all ~f:is_pure [ cond ; bt ; bf ]

  | E_let_in (e1, _, _, (_, e2))
    -> List.for_all ~f:is_pure [ e1 ; e2 ]

  | E_tuple exprs
    -> List.for_all ~f:is_pure exprs
  | E_let_tuple (e1, (_, e2))
    -> List.for_all ~f:is_pure [ e1 ; e2 ]
  | E_proj (e, _i, _n)
    -> is_pure e
  | E_update (expr, _i, update, _n)
    -> List.for_all ~f:is_pure [ expr ; update ]

  | E_constant (c)
    -> is_pure_constant c.cons_name && List.for_all ~f:is_pure c.arguments

  | E_global_constant (_hash, _args) ->
    (* hashed code can be impure :( *)
    false
  | E_create_contract _ ->
    (* very not pure *)
    false

  (* I'm not sure about these. Maybe can be tested better? *)
  | E_application _
  | E_iterator _
  | E_fold _
  | E_fold_right _
    -> false

let occurs_count : expression_variable -> expression -> int =
  fun x e ->
  let fvs = Free_variables.expression [] e in
  Free_variables.mem_count x fvs

(* Let "inlining" mean transforming the code:

     let x = e1 in e2

   to:

     e2[e1/x]

   (where the latter signifies substituting e1 for x in e2.)

   Things which can go wrong for inlining:

   - If `e1` is not pure, inlining may fail to preserve semantics.
   - Free variables of `e1` may be shadowed in e2, at usages of `x`. This
     is not a problem if the substitution is capture-avoiding.
   - ?
*)

let is_variable : expression -> bool =
  fun e ->
  match e.content with
  | E_variable _ -> true
  | _ -> false

let should_inline : expression_variable -> expression -> expression -> bool =
  fun x e1 e2 ->
  occurs_count x e2 <= 1 || is_variable e1

let inline_let : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_let_in (e1, should_inline_here, is_thunk, ((x, _a), e2)) ->
    if (is_pure e1 && (should_inline_here || should_inline x e1 e2)) ||
       is_thunk
    then
      let e2' = Subst.subst_expression ~body:e2 ~x:x ~expr:e1 in
      (changed := true ; e2')
    else
      e
  | _ -> e

let inline_lets : bool ref -> expression -> expression =
  fun changed ->
  map_expression (inline_let changed)


(* Let "beta" mean transforming the code:

     (\x. e1) e2

   to:

     let x = e2 in e1

   Things which can go wrong for beta reduction:

   - Nothing?
*)

let beta : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_application ({ content = E_closure { binder = x ; body = e1 } ; type_expression = {type_content = T_function (xtv, tv);_ }; location = _}, e2) ->
    (changed := true ;
     Expression.make (E_let_in (e2, false, false, ((x, xtv), e1))) tv)

  (* also do CAR (PAIR x y) ↦ x, or CDR (PAIR x y) ↦ y, only if x and y are pure *)
  | E_constant {cons_name = C_CAR| C_CDR as const; arguments = [ { content = E_constant {cons_name = C_PAIR; arguments = [ e1 ; e2 ]} ; type_expression = _ ; location = _} ]} ->
    if is_pure e1 && is_pure e2
    then (changed := true ;
          match const with
          | C_CAR -> e1
          | C_CDR -> e2
          | _ -> assert false)
    else e

  (* (e0, e1, ...).(i) ↦ ei  (only if all ei are pure) *)
  | E_proj ({ content = E_tuple es; _ }, i, _n) ->
    if List.for_all ~f:is_pure es
    then (changed := true;
          List.nth_exn es i)
    else e

  (** (let x = e1 in e2).(i) ↦ (let x = e1 in e2.(i)) *)
  | E_proj ({ content = E_let_in (e1, inline, thunk, ((x, a), e2));type_expression = _; location=_ } as e_let_in, i, n) ->
    changed := true;
    { e_let_in with content = E_let_in (e1, inline, thunk, ((x, a), ({ e with content = E_proj (e2, i, n) }))) ;
                    type_expression = e.type_expression }

  (** (let (x, y, ...) = e1 in e2).(i) ↦ (let (x, y, ...) = e1 in e2.(i)) *)
  | E_proj ({ content = E_let_tuple (e1, (vars, e2));type_expression = _; location=_ } as e_let_tuple, i, n) ->
    changed := true;
    { e_let_tuple with content = E_let_tuple (e1, (vars, ({ e with content = E_proj (e2, i, n) }))) ;
                       type_expression = e.type_expression }

  (** (let x = (let y = e1 in e2) in e3) ↦ (let y = e1 in let x = e2 in e3) *)
  | E_let_in ({ content = E_let_in (e1, inline2, thunk2, ((y, b), e2)); _ }, inline1, thunk1, ((x, a), e3)) ->
    let y' = ValueVar.fresh_like y in
    let e2 = Subst.replace e2 y y' in
    changed := true;
    {e with content = E_let_in (e1, inline2, thunk2, ((y', b), {e with content = E_let_in (e2, inline1, thunk1, ((x, a), e3))}))}

  (** note: E_let_tuple/E_let_in and E_let_in/E_let_tuple conversions
      not implemented yet because they don't seem important (?) *)

  (** (let x = e1 in e2)@e3 ↦ let x = e1 in e2@e3  (if e1 or e3 is pure) *)
  | E_application ({ content = E_let_in (e1, inline, thunk, ((x, a), e2)); _ }, e3) ->
    if is_pure e1 || is_pure e3
    then
      let x' = ValueVar.fresh_like x in
      let e2 = Subst.replace e2 x x' in
      changed := true;
      {e with content = E_let_in (e1, inline, thunk, ((x', a), {e with content = E_application (e2, e3)}))}
    else e

  (** (let (x, y, ...) = e1 in e2)@e3 ↦ let (x, y, ...) = e1 in e2@e3  (if e1 or e3 is pure) *)
  | E_application ({ content = E_let_tuple (e1, (vars, e2)); _ }, e3) ->
    if is_pure e1 || is_pure e3
    then
      let vars = List.map ~f:(fun (x, a) -> (x, ValueVar.fresh_like x, a)) vars in
      let e2 = List.fold_left vars ~init:e2 ~f:(fun e2 (x, x', _a) -> Subst.replace e2 x x') in
      let vars = List.map ~f:(fun (_x, x', a) -> (x', a)) vars in
      changed := true;
      {e with content = E_let_tuple (e1, (vars, {e with content = E_application (e2, e3)}))}
    else e

  (* let (x0, x1, ...) = (e0, e1, ...) in body ↦
     let ... in let x1 = e1 in let x0 = e0 in body
     (here, purity of the ei does not matter)
     *)
  | E_let_tuple ({ content = E_tuple es; _ }, (vars, body))
  | E_let_tuple ({ content = E_constant { cons_name = C_PAIR ; arguments = es }; _ }, (vars, body)) ->
    changed := true;
    List.fold_left
      ~f:(fun body (e, (v, t)) ->
         { content = E_let_in (e, false, false, ((v, t), body));
           location = Location.generated;
           type_expression = body.type_expression })
      ~init:body (List.zip_exn es vars)
  | _ -> e

let betas : bool ref -> expression -> expression =
  fun changed ->
  map_expression (beta changed)

let eta : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_constant {cons_name = C_PAIR; arguments = [ { content = E_constant {cons_name = C_CAR; arguments = [ e1 ]} ; type_expression = _ ; location = _} ;
                                                  { content = E_constant {cons_name = C_CDR; arguments = [ e2 ]} ; type_expression = _ ; location = _}]} ->
    (match (e1.content, e2.content) with
     | E_variable x1, E_variable x2 ->
       if ValueVar.equal x1 x2
       then
         (changed := true;
          { e with content = e1.content })
       else e
     | _ -> e)
  (* (x.(0), x.(1), ...) ↦ x *)
  | E_tuple es ->
    let count = List.length es in
    let projs =
      List.mapi
        ~f:(fun i e ->
           match e.content with
           | E_proj (e', j, n) ->
             if i = j && n = count
             then
               match e'.content with
               | E_variable x -> Some x
               | _ -> None
             else None
           | _ -> None)
        es in
    (match Option.all projs with
     | None -> e
     | Some vars ->
       match vars with
       | var :: _ ->
         if List.for_all ~f:(ValueVar.equal var) vars
         then { e with content = E_variable var }
         else e
       | _ -> e)
  | _ -> e

let etas : bool ref -> expression -> expression =
  fun changed ->
  map_expression (eta changed)
let contract_check ~raise (init: anon_function) : anon_function=
  let all = [Michelson_restrictions.self_in_lambdas ~raise] in
  let all_e = List.map ~f:(Helpers.map_sub_level_expression) all in
  List.fold ~f:(|>) all_e ~init
let rec all_expression ~raise (options : Compiler_options.t) : expression -> expression =
  fun e ->
  if options.backend.experimental_disable_optimizations_for_debugging
  then e
  else
    let changed = ref false in
    let e = inline_lets changed e in
    let e = betas changed e in
    let e = etas changed e in
    if !changed
    then all_expression ~raise options e
    else e

let create_contract ~raise expr =
  let _ = map_expression (fun expr ->
                  match expr.content with
                  | E_create_contract (_, _, ((x, _), lambda), _) -> (
                    let fvs = Free_variables.expression [x] lambda in
                    if Int.equal (List.length fvs) 0 then expr
                    else raise.error @@ fvs_in_create_contract_lambda expr (List.hd_exn fvs)
                  )
                  | _ -> expr) expr in
  expr

let all_expression ~raise options e =
  let e = Uncurry.uncurry_expression e in
  let e = all_expression ~raise options e in
  let e = create_contract ~raise e in
  e
