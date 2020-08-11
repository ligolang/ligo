module Errors = Errors
open Errors
open Mini_c
open Trace

let get_t_function e = trace_option not_a_function @@ Mini_c.get_t_function e
let get_function e = trace_option not_a_function @@ Mini_c.get_function e
let aggregate_entry p f = trace_option could_not_aggregate_entry @@ Mini_c.aggregate_entry p f
let get_entry l n = trace_option could_not_aggregate_entry @@ Mini_c.get_entry l n

(* TODO hack to specialize map_expression to identity monad *)
let map_expression :
  (expression -> expression) -> (expression -> expression) =
  fun f e ->
  match Helpers.map_expression (fun e -> ok (f e)) e with
  | Ok (e, _) -> e
  | Error _ -> assert false (* impossible *)


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
  | C_SOME
  | C_UPDATE | C_MAP_FIND_OPT | C_MAP_ADD | C_MAP_UPDATE
  | C_INT | C_ABS | C_IS_NAT
  | C_ADDRESS
  | C_SET_MEM | C_SET_ADD | C_SET_REMOVE | C_SLICE
  | C_SHA256 | C_SHA512 | C_BLAKE2b | C_CHECK_SIGNATURE
  | C_HASH_KEY | C_BYTES_PACK | C_CONCAT
  | C_NONE
  | C_FOLD_CONTINUE | C_FOLD_STOP
  | C_LOOP_CONTINUE | C_LOOP_STOP
  | C_EDIV
  | C_SIZE
  | C_BYTES_UNPACK
  | C_LEFT | C_RIGHT
  | C_SET_EMPTY | C_SET_LITERAL
  | C_LIST_EMPTY | C_LIST_LITERAL
  | C_MAP_EMPTY | C_MAP_LITERAL
  | C_MAP_GET | C_MAP_REMOVE | C_MAP_MEM
  | C_CONVERT_TO_LEFT_COMB | C_CONVERT_TO_RIGHT_COMB
  | C_CONVERT_FROM_LEFT_COMB | C_CONVERT_FROM_RIGHT_COMB
    -> true
  (* unfortunately impure: *)
  | C_BALANCE | C_AMOUNT | C_NOW | C_SOURCE | C_SENDER | C_CHAIN_ID
  | C_ADD | C_SUB |C_MUL|C_DIV|C_MOD | C_LSL | C_LSR 
  (* impure: *)
  | C_ASSERTION
  | C_ASSERT_INFERRED
  | C_MAP_FIND
  | C_FOLD_WHILE
  | C_CALL
  | C_FAILWITH
  | C_ITER
  | C_LOOP_LEFT
  | C_FOLD
  | C_SET_ITER
  | C_SET_FOLD
  | C_LIST_ITER
  | C_LIST_MAP
  | C_LIST_FOLD
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
  | C_HASH
  | C_CONTRACT
  | C_CONTRACT_OPT
  | C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT
  | C_SELF
  | C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT
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
    -> List.for_all is_pure [ cond ; bt ; bf ]

  | E_let_in (_, _, e1, e2)
    -> List.for_all is_pure [ e1 ; e2 ]

  | E_constant (c)
    -> is_pure_constant c.cons_name && List.for_all is_pure c.arguments
  | E_record_update (e, _,up)
    -> is_pure e && is_pure up

  (* I'm not sure about these. Maybe can be tested better? *)
  | E_application _
  | E_iterator _
  | E_fold _
    -> false

let occurs_in : expression_variable -> expression -> bool =
  fun x e ->
  let fvs = Free_variables.expression [] e in
  Free_variables.mem x fvs

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
  | E_let_in ((x, _a), should_inline_here, e1, e2) ->
    if is_pure e1 && (should_inline_here || should_inline x e1 e2)
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
  | E_application ({ content = E_closure { binder = x ; body = e1 } ; type_expression = {type_content = T_function (xtv, tv);_ }}, e2) ->
    (changed := true ;
     Expression.make (E_let_in ((x, xtv), false, e2, e1)) tv)

  (* also do CAR (PAIR x y) ↦ x, or CDR (PAIR x y) ↦ y, only if x and y are pure *)
  | E_constant {cons_name = C_CAR| C_CDR as const; arguments = [ { content = E_constant {cons_name = C_PAIR; arguments = [ e1 ; e2 ]} ; type_expression = _ } ]} ->
    if is_pure e1 && is_pure e2
    then (changed := true ;
          match const with
          | C_CAR -> e1
          | C_CDR -> e2
          | _ -> assert false)
    else e
  | _ -> e

let betas : bool ref -> expression -> expression =
  fun changed ->
  map_expression (beta changed)

let eta : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_constant {cons_name = C_PAIR; arguments = [ { content = E_constant {cons_name = C_CAR; arguments = [ e1 ]} ; type_expression = _ } ;
                                                  { content = E_constant {cons_name = C_CDR; arguments = [ e2 ]} ; type_expression = _ }]} ->
    (match (e1.content, e2.content) with
     | E_variable x1, E_variable x2 ->
       if Var.equal x1.wrap_content x2.wrap_content
       then
         (changed := true;
          { e with content = e1.content })
       else e
     | _ -> e)
  | _ -> e

let etas : bool ref -> expression -> expression =
  fun changed ->
  map_expression (eta changed)

let contract_check =
  let all = [Michelson_restrictions.self_in_lambdas] in
  let all_e = List.map Helpers.map_sub_level_expression all in
  bind_chain all_e

let rec all_expression : expression -> expression =
  fun e ->
  let changed = ref false in
  let e = inline_lets changed e in
  let e = betas changed e in
  let e = etas changed e in
  if !changed
  then all_expression e
  else e
