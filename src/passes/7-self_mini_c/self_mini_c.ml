open Mini_c
open Trace

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
let is_pure_constant : constant -> bool =
  function
  | C_UNIT
  | C_CAR | C_CDR | C_PAIR
  | C_NIL | C_CONS
  | C_NEG | C_OR | C_AND | C_XOR | C_NOT
  | C_EQ  | C_NEQ | C_LT | C_LE | C_GT | C_GE
  | C_SOME
  | C_UPDATE | C_MAP_GET | C_MAP_FIND_OPT | C_MAP_ADD | C_MAP_UPDATE
  | C_INT | C_ABS | C_IS_NAT
  | C_BALANCE | C_AMOUNT | C_ADDRESS | C_NOW | C_SOURCE | C_SENDER | C_CHAIN_ID
  | C_SET_MEM | C_SET_ADD | C_SET_REMOVE | C_SLICE
  | C_SHA256 | C_SHA512 | C_BLAKE2b | C_CHECK_SIGNATURE
  | C_HASH_KEY | C_BYTES_PACK | C_CONCAT
    -> true
  (* unfortunately impure: *)
  | C_ADD | C_SUB |C_MUL|C_DIV|C_MOD
  (* impure: *)
  | C_ASSERTION | C_ASSERT_INFERRED
  | C_MAP_GET_FORCE | C_MAP_FIND
  | C_FOLD_WHILE
  | C_CALL
  (* TODO... *)
  | _
    -> false

let rec is_pure : expression -> bool = fun e ->
  match e.content with
  | E_literal _
  | E_closure _
  | E_skip
  | E_variable _
  | E_make_empty_map _
  | E_make_empty_big_map _
  | E_make_empty_list _
  | E_make_empty_set _
  | E_make_none _
    -> true

  | E_if_bool (cond, bt, bf)
  | E_if_none (cond, bt, (_, bf))
  | E_if_cons (cond, bt, (_, bf))
  | E_if_left (cond, (_, bt), (_, bf))
    -> List.for_all is_pure [ cond ; bt ; bf ]

  | E_let_in (_, _, e1, e2)
  | E_sequence (e1, e2)
    -> List.for_all is_pure [ e1 ; e2 ]

  | E_constant (c, args)
    -> is_pure_constant c && List.for_all is_pure args
  | E_update (r, (_,e))
    -> is_pure r && is_pure e

  (* I'm not sure about these. Maybe can be tested better? *)
  | E_application _
  | E_iterator _
  | E_fold _
    -> false

  (* Could be pure, but, divergence is an effect, so halting problem
     is near... *)
  | E_while _ -> false

  (* definitely not pure *)
  | E_assignment _ -> false

let occurs_in : expression_variable -> expression -> bool =
  fun x e ->
  let fvs = Free_variables.expression [] e in
  Free_variables.mem x fvs

let occurs_count : expression_variable -> expression -> int =
  fun x e ->
  let fvs = Free_variables.expression [] e in
  Free_variables.mem_count x fvs

(* If `ignore_lambdas` is true, ignore assignments which occur inside
   lambdas, which have no effect on the value of the variable outside
   of the lambda. *)
let rec is_assigned : ignore_lambdas:bool -> expression_variable -> expression -> bool =
  fun ~ignore_lambdas x e ->
  let self = is_assigned ~ignore_lambdas x in
  let selfs = List.exists self in
  let it = Var.equal x in
  let self_binder binder body =
    if it binder
    then false
    else self body in
  let self_binder2 binder1 binder2 body =
    if it binder1 || it binder2
    then false
    else self body in
  match e.content with
  | E_assignment (x, _, e) ->
    it x || self e
  | E_update (r, (_,e)) ->
    self r || self e
  | E_closure { binder; body } ->
    if ignore_lambdas
    then false
    else self_binder binder body
  | E_constant (_, args) ->
    selfs args
  | E_application (f, arg) ->
    selfs [ f ; arg ]
  | E_iterator (_, ((x, _), e1), e2) ->
    self_binder x e1 || self e2
  | E_fold (((x, _), e1), e2, e3) ->
    self_binder x e1 || selfs [ e2 ; e3 ]
  | E_if_bool (e1, e2, e3) ->
    selfs [ e1 ; e2 ; e3 ]
  | E_if_none (e1, e2, ((x, _), e3)) ->
    selfs [ e1 ; e2 ] || self_binder x e3
  | E_if_cons (e1, e2, (((hd, _), (tl, _)), e3)) ->
    selfs [ e1 ; e2 ] || self_binder2 hd tl e3
  | E_if_left (e1, ((l, _), e2), ((r, _), e3)) ->
    self e1 || self_binder l e2 || self_binder r e3
  | E_let_in ((x, _), _, e1, e2) ->
    self e1 || self_binder x e2
  | E_sequence (e1, e2) ->
    selfs [ e1 ; e2 ]
  | E_while (e1, e2) ->
    selfs [ e1 ; e2 ]
  | E_literal _
  | E_skip
  | E_variable _
  | E_make_empty_map _
  | E_make_empty_big_map _
  | E_make_empty_list _
  | E_make_empty_set _
  | E_make_none _ ->
    false

(* Let "inlining" mean transforming the code:

     let x = e1 in e2

   to:

     e2[e1/x]

   (where the latter signifies substituting e1 for x in e2.)

   Things which can go wrong for inlining:

   - If `e1` is not pure, inlining may fail to preserve semantics.
   - If assignments to `x` occur in e2, inlining does not make sense.
   - Free variables of `e1` may be assigned in e2, before usages of `x`.
   - Free variables of `e1` may be shadowed in e2, at usages of `x`. This
     is not a problem if the substitution is capture-avoiding.
   - ?
*)

let can_inline : expression_variable -> expression -> expression -> bool =
  fun x e1 e2 ->
  is_pure e1 &&
  (* if x does not occur in e2, there can be no other problems:
     substitution will be a noop up to alpha-equivalence *)
  (not (occurs_in x e2) ||
   (* else, must worry about assignment *)
   (not (is_assigned ~ignore_lambdas:false x e2) &&
    List.for_all
      (fun y -> not (is_assigned ~ignore_lambdas:true y e2))
      (Free_variables.expression [] e2)))

let should_inline : expression_variable -> expression -> bool =
  fun x e ->
  occurs_count x e <= 1

let inline_let : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_let_in ((x, _a), should_inline_here, e1, e2) ->
    if can_inline x e1 e2 && (should_inline_here || should_inline x e2)
    then
      (* can raise Subst.Bad_argument, but should not happen, due to
         can_inline *)
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

   - If e1 contains (meaningful) assignments to free variables, semantics
     will not be preserved.
   - ?
*)

let can_beta : anon_function -> bool =
  fun lam ->
  List.for_all
    (fun x -> not (is_assigned ~ignore_lambdas:true x lam.body))
    (Free_variables.lambda [] lam)

let beta : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_application ({ content = E_closure { binder = x ; body = e1 } ; type_value = T_function (xtv, tv) }, e2) ->
    if can_beta { binder = x ; body = e1 }
    then
      (changed := true ;
       Expression.make (E_let_in ((x, xtv), false, e2, e1)) tv)
    else e

  (* also do CAR (PAIR x y) ↦ x, or CDR (PAIR x y) ↦ y, only if x and y are pure *)
  | E_constant (C_CAR| C_CDR as const, [ { content = E_constant (C_PAIR, [ e1 ; e2 ]) ; type_value = _ } ]) ->
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

let contract_check =
  let all = [Michelson_restrictions.self_in_lambdas] in
  let all_e = List.map Helpers.map_sub_level_expression all in
  bind_chain all_e

let rec all_expression : expression -> expression =
  fun e ->
  let changed = ref false in
  let e = inline_lets changed e in
  let e = betas changed e in
  if !changed
  then all_expression e
  else e
