open Trace

module Core = Typesystem.Core

module Wrap = Wrap
open Wrap

module TypeVariable =
struct
  type t = Core.type_variable
  let compare a b = Var.compare a b
  let to_string = (fun s -> Format.asprintf "%a" Var.pp s)

end

module UF = UnionFind.Partition0.Make(TypeVariable)

type unionfind = UF.t

(* end unionfind *)

(* representant for an equivalence class of type variables *)
module TypeVariableMap = Map.Make(TypeVariable)


(*

Components:
* assignments (passive data structure).
  Now: just a map from unification vars to types (pb: what about partial types?)
  maybe just local assignments (allow only vars as children of pair(α,β))
* constraint propagation: (buch of constraints) → (new constraints * assignments)
  * sub-component: constraint selector (worklist / dynamic queries)
    * sub-sub component: constraint normalizer: remove dupes and give structure
      right now: union-find of unification vars
      later: better database-like organisation of knowledge
    * sub-sub component: lazy selector (don't re-try all selectors every time)
      For now: just re-try everytime
  * sub-component: propagation rule
    For now: break pair(a, b) = pair(c, d) into a = c, b = d
* generalizer
  For now: ?

Workflow:
  Start with empty assignments and structured database
  Receive a new constraint
  For each normalizer:
    Use the pre-selector to see if it can be applied
    Apply the normalizer, get some new items to insert in the structured database
  For each propagator:
    Use the selector to query the structured database and see if it can be applied
    Apply the propagator, get some new constraints and assignments
  Add the new assignments to the data structure.

  At some point (when?)
  For each generalizer:
    Use the generalizer's selector to see if it can be applied
    Apply the generalizer to produce a new type, possibly with some ∀s injected

*)

open Core

type structured_dbs = {
  all_constraints     : type_constraint_simpl list ;
  aliases             : unionfind ;
  (* assignments (passive data structure).
     Now: just a map from unification vars to types (pb: what about partial types?)
     maybe just local assignments (allow only vars as children of pair(α,β)) *)
  (* TODO: the rhs of the map should not repeat the variable name. *)
  assignments         : c_constructor_simpl TypeVariableMap.t ;
  grouped_by_variable : constraints TypeVariableMap.t ; (* map from (unionfind) variables to constraints containing them *)
  cycle_detection_toposort : unit ; (* example of structured db that we'll add later *)
}

and constraints = {
  (* If implemented in a language with decent sets, these should be sets not lists. *)
  constructor : c_constructor_simpl list ; (* List of ('a = constructor(args…)) constraints *)
  poly        : c_poly_simpl        list ; (* List of ('a = forall 'b, some_type) constraints *)
  tc          : c_typeclass_simpl   list ; (* List of (typeclass(args…)) constraints *)
}

and c_constructor_simpl = {
  tv : type_variable;
  c_tag : constant_tag;
  tv_list : type_variable list;
}
(* copy-pasted from core.ml *)
and c_const = (type_variable * type_expression)
and c_equation = (type_expression * type_expression)
and c_typeclass_simpl = {
  tc   : typeclass          ;
  args : type_variable list ;
}
and c_poly_simpl = {
  tv     : type_variable ;
  forall : p_forall      ;
}
and type_constraint_simpl =
    SC_Constructor of c_constructor_simpl             (* α = ctor(β, …) *)
  | SC_Alias       of (type_variable * type_variable) (* α = β *)
  | SC_Poly        of c_poly_simpl                    (* α = forall β, δ where δ can be a more complex type *)
  | SC_Typeclass   of c_typeclass_simpl               (* TC(α, …) *)

module UnionFindWrapper = struct
  (* Light wrapper for API for grouped_by_variable in the structured
     db, to access it modulo unification variable aliases. *)
  let get_constraints_related_to : type_variable -> structured_dbs -> constraints =
    fun variable dbs ->
      let variable , aliases = UF.get_or_set variable dbs.aliases in
      let dbs = { dbs with aliases } in
      match TypeVariableMap.find_opt variable dbs.grouped_by_variable with
        Some l -> l
      | None -> {
          constructor = [] ;
          poly        = [] ;
          tc          = [] ;
        }
  let add_constraints_related_to : type_variable -> constraints -> structured_dbs -> structured_dbs =
    fun variable c dbs ->
      (* let (variable_repr , _height) , aliases = UF.get_or_set variable dbs.aliases in
         let dbs = { dbs with aliases } in *)
      let variable_repr , aliases = UF.get_or_set variable dbs.aliases in
      let dbs = { dbs with aliases } in
      let grouped_by_variable = TypeVariableMap.update variable_repr (function
            None -> Some c
          | Some x -> Some {
              constructor = c.constructor @ x.constructor ;
              poly        = c.poly        @ x.poly        ;
              tc          = c.tc          @ x.tc          ;
            })
          dbs.grouped_by_variable
      in
      let dbs = { dbs with grouped_by_variable } in
      dbs

  let merge_constraints : type_variable -> type_variable -> structured_dbs -> structured_dbs =
    fun variable_a variable_b dbs ->
    (* get old representant for variable_a *)
    let variable_repr_a , aliases = UF.get_or_set variable_a dbs.aliases in
    let dbs = { dbs with aliases } in
    (* get old representant for variable_b *)
    let variable_repr_b , aliases = UF.get_or_set variable_b dbs.aliases in
    let dbs = { dbs with aliases } in

    (* alias variable_a and variable_b together *)
    let aliases = UF.alias variable_a variable_b dbs.aliases in
    let dbs = { dbs with aliases } in

    (* Replace the two entries in grouped_by_variable by a single one *)
    (
      let get_constraints ab =
        match TypeVariableMap.find_opt ab dbs.grouped_by_variable with
        | Some x -> x
        | None -> { constructor = [] ; poly = [] ; tc = [] } in
      let constraints_a = get_constraints variable_repr_a in
      let constraints_b = get_constraints variable_repr_b in
      let all_constraints = {
          constructor = constraints_a.constructor @ constraints_b.constructor ;
          poly        = constraints_a.poly        @ constraints_b.poly        ;
          tc          = constraints_a.tc          @ constraints_b.tc          ;
        } in
      let grouped_by_variable =
        TypeVariableMap.add variable_repr_a all_constraints dbs.grouped_by_variable in
      let dbs = { dbs with grouped_by_variable} in
      let grouped_by_variable =
        TypeVariableMap.remove variable_repr_b dbs.grouped_by_variable in
      let dbs = { dbs with grouped_by_variable} in
      dbs
    )
end

(* sub-sub component: constraint normalizer: remove dupes and give structure
 * right now: union-find of unification vars
 * later: better database-like organisation of knowledge *)

(* Each normalizer returns an updated database (after storing the
   incoming constraint) and a list of constraints, used when the
   normalizer rewrites the constraints e.g. into simpler ones. *)
(* TODO: If implemented in a language with decent sets, should be 'b set not 'b list. *)
type ('a , 'b) normalizer = structured_dbs -> 'a -> (structured_dbs * 'b list)

(** Updates the dbs.all_constraints field when new constraints are
   discovered.

   This field contains a list of all the constraints, without any form of
   grouping or sorting. *)
let normalizer_all_constraints : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
    ({ dbs with all_constraints = new_constraint :: dbs.all_constraints } , [new_constraint])

(** Updates the dbs.grouped_by_variable field when new constraints are
   discovered.

    This field contains a map from type variables to lists of
   constraints that are related to that variable (in other words, the
   key appears in the equation).
 *)
let normalizer_grouped_by_variable : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
  let store_constraint tvars constraints =
    let aux dbs (tvar : type_variable) =
      UnionFindWrapper.add_constraints_related_to tvar constraints dbs
    in List.fold_left aux dbs tvars
  in
  let dbs = match new_constraint with
      SC_Constructor ({tv ; c_tag = _ ; tv_list} as c) -> store_constraint (tv :: tv_list) {constructor = [c] ; poly = []  ; tc = []}
    | SC_Typeclass   ({tc = _ ; args}            as c) -> store_constraint args            {constructor = []  ; poly = []  ; tc = [c]}
    | SC_Poly        ({tv; forall = _}           as c) -> store_constraint [tv]            {constructor = []  ; poly = [c] ; tc = []}
    | SC_Alias (a , b) -> UnionFindWrapper.merge_constraints a b dbs
  in (dbs , [new_constraint])

(** Stores the first assinment ('a = ctor('b, …)) that is encountered.

    Subsequent ('a = ctor('b2, …)) with the same 'a are ignored.

    TOOD: are we checking somewhere that 'b … = 'b2 … ? *)
let normalizer_assignments : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
    match new_constraint with
    | SC_Constructor ({tv ; c_tag = _ ; tv_list = _} as c) ->
      let assignments = TypeVariableMap.update tv (function None -> Some c | e -> e) dbs.assignments in
      let dbs = {dbs with assignments} in
      (dbs , [new_constraint])
    | _ ->
      (dbs , [new_constraint])

(** Evaluates a type-leval application. For now, only supports
    immediate beta-reduction at the root of the type. *)
let type_level_eval : type_value -> type_value * type_constraint list =
  fun tv -> Typesystem.Misc.Substitution.Pattern.eval_beta_root ~tv

(** Checks that a type-level application has been fully reduced. For
    now, only some simple cases like applications of `forall`
    <polymorphic types are allowed. *)
let check_applied ((reduced, _new_constraints) as x) =
  let () = match reduced with
      P_apply _ -> failwith "internal error: shouldn't happen" (* failwith "could not reduce type-level application. Arbitrary type-level applications are not supported for now." *)
    | _ -> ()
  in x

(* TODO: at some point there may be uses of named type aliases (type
   foo = int; let x : foo = 42). These should be inlined. *)

(** This function converts constraints from type_constraint to
    type_constraint_simpl. The former has more possible cases, and the
    latter uses a more minimalistic constraint language.

    It does not modify the dbs, and only rewrites the constraint

    TODO: update the code to show that the dbs are always copied as-is
 *)
let rec normalizer_simpl : (type_constraint , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
  let insert_fresh a b =
    let fresh = Core.fresh_type_variable () in
    let (dbs , cs1) = normalizer_simpl dbs (C_equation (P_variable fresh, a)) in
    let (dbs , cs2) = normalizer_simpl dbs (C_equation (P_variable fresh, b)) in
    (dbs , cs1 @ cs2) in
  let split_constant a c_tag args =
    let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map (fun (v,t) -> C_equation (P_variable v, t)) (List.combine fresh_vars args) in
    let (dbs , recur) = List.fold_map_acc normalizer_simpl dbs fresh_eqns in
    (dbs , [SC_Constructor {tv=a;c_tag;tv_list=fresh_vars}] @ List.flatten recur) in
  let gather_forall a forall = (dbs , [SC_Poly { tv=a; forall }]) in
  let gather_alias a b = (dbs , [SC_Alias (a, b)]) in
  let reduce_type_app a b =
    let (reduced, new_constraints) = check_applied @@ type_level_eval b in
    let (dbs , recur) = List.fold_map_acc normalizer_simpl dbs new_constraints in
    let (dbs , resimpl) = normalizer_simpl dbs (C_equation (a , reduced)) in (* Note: this calls recursively but cant't fall in the same case. *)
    (dbs , resimpl @ List.flatten recur) in
  let split_typeclass args tc =
    let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map (fun (v,t) -> C_equation (P_variable v, t)) (List.combine fresh_vars args) in
    let (dbs , recur) = List.fold_map_acc normalizer_simpl dbs fresh_eqns in
    (dbs, [SC_Typeclass { tc ; args = fresh_vars }] @ List.flatten recur) in

  match new_constraint with
  (* break down (forall 'b, body = forall 'c, body') into ('a = forall 'b, body and 'a = forall 'c, body')) *)
  | C_equation ((P_forall _ as a), (P_forall _ as b))     -> insert_fresh a b
  (* break down (forall 'b, body = c(args)) into ('a = forall 'b, body and 'a = c(args)) *)
  | C_equation ((P_forall _ as a), (P_constant _ as b))   -> insert_fresh a b
  (* break down (c(args) = c'(args')) into ('a = c(args) and 'a = c'(args')) *)
  | C_equation ((P_constant _ as a), (P_constant _ as b)) -> insert_fresh a b
  (* break down (c(args) = forall 'b, body) into ('a = c(args) and 'a = forall 'b, body) *)
  | C_equation ((P_constant _ as a), (P_forall _ as b))   -> insert_fresh a b
  | C_equation ((P_forall forall), (P_variable b))        -> gather_forall b forall
  | C_equation (P_variable a, P_forall forall)            -> gather_forall a forall
  | C_equation (P_variable a, P_variable b)               -> gather_alias a b
  | C_equation (P_variable a, P_constant (c_tag , args))  -> split_constant a c_tag args
  | C_equation (P_constant (c_tag , args), P_variable b)  -> split_constant b c_tag args
  (*  Reduce the type-level application, and simplify the resulting constraint + the extra constraints (typeclasses) that appeared at the forall binding site *)
  | C_equation ((_ as a), (P_apply _ as b))               -> reduce_type_app a b
  | C_equation ((P_apply _ as a), (_ as b))               -> reduce_type_app b a
  (* break down (TC(args)) into (TC('a, …) and ('a = arg) …) *)
  | C_typeclass (args, tc)                                -> split_typeclass args tc
  | C_access_label (tv, label, result)                    -> let _todo = ignore (tv, label, result) in failwith "TODO"

(* Random notes from live discussion. Kept here to include bits as a rationale later on / remind me of the discussion in the short term.
 * Feel free to erase if it rots here for too long.
 *
 * function (zetype, zevalue) { if (typeof(zevalue) != zetype) { ohlàlà; } else { return zevalue; } }
 * 
 * let f = (fun {a : Type} (v : a) -> v)
 * 
 * (forall 'a, 'a -> 'a) ~ (int -> int)
 * (forall {a : Type}, forall (v : a), a) ~ (forall (v : int), int)
 * ({a : Type} -> (v : a) -> a) ~ ((v : int) -> int)
 * 
 * (@f int)
 * 
 * 
 *              'c   'c
 *              'd -> 'e  && 'c ~ d && 'c ~ 'e
 *              'c -> 'c  ???????????????wtf---->???????????? [ scope of 'c is fun z ]
 *   'tid ~ (forall 'c, 'c -> 'c)
 * let id = (fun z -> z) in
 * let ii = (fun z -> z + 0) : (int -> int) in
 * 
 *       'a 'b   ['a ~ 'b]      'a     'b
 *       'a 'a   'a             'a     'a
 *   (forall 'a, 'a -> 'a   -> 'a        ) 'tid                     'tid
 * 
 *             'tid -> 'tid -> 'tid
 * 
 *   (forall 'a, 'a -> 'a   -> 'a        ) (forall 'c1, 'c1 -> 'c1) (int -> int)
 *                                         (forall 'c1, 'c1 -> 'c1)~(int -> int)
 *                                         ('c1 -> 'c1) ~ (int -> int)
 *   (fun x  y -> if random then x else y) id                       ii as toto
 *   id "foo" *)

type ('state, 'elt) state_list_monad = { state: 'state ; list : 'elt list }
let lift_state_list_monad ~state ~list = { state ; list }
let lift f =
  fun { state ; list } ->
    let (new_state , new_lists) = List.fold_map_acc f state list in
    { state = new_state ; list = List.flatten new_lists }

(* TODO: move this to the List module *)
let named_fold_left f ~acc ~lst = List.fold_left (fun acc elt -> f ~acc ~elt) acc lst

module Fun = struct let id x = x end (* in stdlib as of 4.08, we're in 4.07 for now *)

let normalizers : type_constraint -> structured_dbs -> (structured_dbs , 'modified_constraint) state_list_monad =
  fun new_constraint dbs ->
    Fun.id
    @@ lift normalizer_grouped_by_variable
    @@ lift normalizer_assignments
    @@ lift normalizer_all_constraints
    @@ lift normalizer_simpl
    @@ lift_state_list_monad ~state:dbs ~list:[new_constraint]

(* sub-sub component: lazy selector (don't re-try all selectors every time)
 * For now: just re-try everytime *)

type 'old_constraint_type selector_input = 'old_constraint_type (* some info about the constraint just added, so that we know what to look for *)
type 'selector_output selector_outputs =
    WasSelected of 'selector_output list
  | WasNotSelected
type new_constraints = type_constraint list
type new_assignments = c_constructor_simpl list

type ('old_constraint_type, 'selector_output) selector = 'old_constraint_type selector_input -> structured_dbs -> 'selector_output selector_outputs
type 'selector_output propagator = 'selector_output -> structured_dbs -> new_constraints * new_assignments

(* selector / propagation rule for breaking down composite types
 * For now: break pair(a, b) = pair(c, d) into a = c, b = d *)

type output_break_ctor = { a_k_var : c_constructor_simpl ; a_k'_var' : c_constructor_simpl }
let selector_break_ctor :  (type_constraint_simpl, output_break_ctor) selector =
  (* find two rules with the shape a = k(var …) and a = k'(var' …) *)
  fun type_constraint_simpl dbs ->
  match type_constraint_simpl with
    SC_Constructor c ->
    (* finding other constraints related to the same type variable and
       with the same sort of constraint (constructor vs. constructor)
       is symmetric *)
    let other_cs = (UnionFindWrapper.get_constraints_related_to c.tv dbs).constructor in
    let cs_pairs = List.map (fun x -> { a_k_var = c ; a_k'_var' = x }) other_cs in
    WasSelected cs_pairs
  | SC_Alias       _                -> WasNotSelected (* TODO: ??? (beware: symmetry) *)
  | SC_Poly        _                -> WasNotSelected (* TODO: ??? (beware: symmetry) *)
  | SC_Typeclass   _                -> WasNotSelected

(* TODO: move this to a more appropriate place and/or auto-generate it. *)
let compare_simple_c_constant = function
  | C_arrow -> (function
      (* N/A -> 1 *)
      | C_arrow -> 0
      | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_option -> (function
      | C_arrow -> 1
      | C_option -> 0
      | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_record -> (function
      | C_arrow | C_option  -> 1
      | C_record -> 0
      | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_variant -> (function
      | C_arrow | C_option  | C_record -> 1
      | C_variant -> 0
      | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_map -> (function
      | C_arrow | C_option  | C_record | C_variant -> 1
      | C_map -> 0
      | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_big_map -> (function
      | C_arrow | C_option  | C_record | C_variant | C_map -> 1
      | C_big_map -> 0
      | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_list -> (function
      | C_arrow | C_option  | C_record | C_variant | C_map | C_big_map -> 1
      | C_list -> 0
      | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_set -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list -> 1
      | C_set -> 0
      | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_unit -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set -> 1
      | C_unit -> 0
      | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_bool -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit -> 1
      | C_bool -> 0
      | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_string -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool -> 1
      | C_string -> 0
      | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_nat -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string -> 1
      | C_nat -> 0
      | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_mutez -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat -> 1
      | C_mutez -> 0
      | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_timestamp -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez -> 1
      | C_timestamp -> 0
      | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_int -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp -> 1
      | C_int -> 0
      | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_address -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int -> 1
      | C_address -> 0
      | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_bytes -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address -> 1
      | C_bytes -> 0
      | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_key_hash -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes -> 1
      | C_key_hash -> 0
      | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_key -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash -> 1
      | C_key -> 0
      | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_signature -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key -> 1
      | C_signature -> 0
      | C_operation | C_contract | C_chain_id -> -1)
  | C_operation -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature -> 1
      | C_operation -> 0
      | C_contract | C_chain_id -> -1)
  | C_contract -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation -> 1
      | C_contract -> 0
      | C_chain_id -> -1)
  | C_chain_id -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit | C_bool | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract -> 1
      | C_chain_id -> 0
      (* N/A -> -1 *)
    )

(* Using a pretty-printer from the PP.ml module creates a dependency
   loop, so the one that we need temporarily for debugging purposes
   has been copied here. *)
let debug_pp_constant : _ -> constant_tag -> unit = fun ppf c_tag ->
    let ct = match c_tag with
      | Core.C_arrow     -> "arrow"
      | Core.C_option    -> "option"
      | Core.C_record    -> failwith "record"
      | Core.C_variant   -> failwith "variant"
      | Core.C_map       -> "map"
      | Core.C_big_map   -> "big_map"
      | Core.C_list      -> "list"
      | Core.C_set       -> "set"
      | Core.C_unit      -> "unit"
      | Core.C_bool      -> "bool"
      | Core.C_string    -> "string"
      | Core.C_nat       -> "nat"
      | Core.C_mutez     -> "mutez"
      | Core.C_timestamp -> "timestamp"
      | Core.C_int       -> "int"
      | Core.C_address   -> "address"
      | Core.C_bytes     -> "bytes"
      | Core.C_key_hash  -> "key_hash"
      | Core.C_key       -> "key"
      | Core.C_signature -> "signature"
      | Core.C_operation -> "operation"
      | Core.C_contract  -> "contract"
      | Core.C_chain_id  -> "chain_id"
    in
    Format.fprintf ppf "%s" ct

let debug_pp_c_constructor_simpl ppf { tv; c_tag; tv_list } =
  Format.fprintf ppf "CTOR %a %a(%a)" Var.pp tv debug_pp_constant c_tag PP_helpers.(list_sep Var.pp (const " , ")) tv_list

let propagator_break_ctor : output_break_ctor propagator =
  fun selected dbs ->
  let () = ignore (dbs) in (* this propagator doesn't need to use the dbs *)
  let a = selected.a_k_var in
  let b = selected.a_k'_var' in
  (* produce constraints: *)

  (* a.tv = b.tv *)
  let eq1 = C_equation (P_variable a.tv, P_variable b.tv) in
  (* a.c_tag = b.c_tag *)
  if (compare_simple_c_constant a.c_tag b.c_tag) <> 0 then
    failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)" debug_pp_c_constructor_simpl a debug_pp_c_constructor_simpl b (compare_simple_c_constant a.c_tag b.c_tag))
  else
    (* a.tv_list = b.tv_list *)
  if List.length a.tv_list <> List.length b.tv_list then
    failwith "type error: incompatible types, not same length"
  else
    let eqs3 = List.map2 (fun aa bb -> C_equation (P_variable aa, P_variable bb)) a.tv_list b.tv_list in
    let eqs = eq1 :: eqs3 in
    (eqs , []) (* no new assignments *)

(* TODO : with our selectors, the selection depends on the order in which the constraints are added :-( :-( :-( :-(
   We need to return a lazy stream of constraints. *)

type output_specialize1 = { poly : c_poly_simpl ; a_k_var : c_constructor_simpl }


let (<?) ca cb =
  if ca = 0 then cb () else ca
let rec compare_list f = function
  | hd1::tl1 -> (function
        [] -> 1
      | hd2::tl2 ->
        f hd1 hd2 <? fun () ->
          compare_list f tl1 tl2)
  | [] -> (function [] -> 0 | _::_ -> -1) (* This follows the behaviour of Pervasives.compare for lists of different length *)
let compare_type_variable a b =
  Var.compare a b
let compare_label (a:accessor) (b:accessor) = 
  let Label a = a in
  let Label b = b in
  String.compare a b
let rec compare_typeclass a b = compare_list (compare_list compare_type_expression) a b
and compare_type_expression = function
  | P_forall { binder=a1; constraints=a2; body=a3 } -> (function
      | P_forall { binder=b1; constraints=b2; body=b3 } ->
        compare_type_variable a1 b1 <? fun () ->
          compare_list compare_type_constraint a2 b2  <? fun () ->
            compare_type_expression a3 b3
      | P_variable _ -> -1
      | P_constant _ -> -1
      | P_apply _ -> -1)
  | P_variable a -> (function
      | P_forall _ -> 1
      | P_variable b -> compare_type_variable a b
      | P_constant _ -> -1
      | P_apply _ -> -1)
  | P_constant (a1, a2) -> (function
      | P_forall _ -> 1
      | P_variable _ -> 1
      | P_constant (b1, b2) -> compare_simple_c_constant a1 b1 <? fun () -> compare_list compare_type_expression a2 b2
      | P_apply _ -> -1)
  | P_apply (a1, a2) -> (function
      | P_forall _ -> 1
      | P_variable _ -> 1
      | P_constant _ -> 1
      | P_apply (b1, b2) -> compare_type_expression a1 b1 <? fun () -> compare_type_expression a2 b2)
and compare_type_constraint = function
  | C_equation (a1, a2) -> (function
      | C_equation (b1, b2) -> compare_type_expression a1 b1 <? fun () -> compare_type_expression a2 b2
      | C_typeclass _ -> -1
      | C_access_label _ -> -1)
  | C_typeclass (a1, a2) -> (function
      | C_equation _ -> 1
      | C_typeclass (b1, b2) -> compare_list compare_type_expression a1 b1 <? fun () -> compare_typeclass a2 b2
      | C_access_label _ -> -1)
  | C_access_label (a1, a2, a3) -> (function
      | C_equation _ -> 1
      | C_typeclass _ -> 1
      | C_access_label (b1, b2, b3) -> compare_type_expression a1 b1 <? fun () -> compare_label a2 b2  <? fun () -> compare_type_variable a3 b3)
let compare_type_constraint_list = compare_list compare_type_constraint
let compare_p_forall
    { binder = a1; constraints = a2; body = a3 }
    { binder = b1; constraints = b2; body = b3 } =
  compare_type_variable a1 b1 <? fun () ->
    compare_type_constraint_list a2 b2 <? fun () ->
      compare_type_expression a3 b3
let compare_c_poly_simpl { tv = a1; forall = a2 } { tv = b1; forall = b2 } =
  compare_type_variable a1 b1 <? fun () ->
    compare_p_forall a2 b2
let compare_c_constructor_simpl { tv=a1; c_tag=a2; tv_list=a3 } { tv=b1; c_tag=b2; tv_list=b3 } =
  compare_type_variable a1 b1 <? fun () -> compare_simple_c_constant a2 b2  <? fun () -> compare_list compare_type_variable a3 b3

let compare_output_specialize1 { poly = a1; a_k_var = a2 } { poly = b1; a_k_var = b2 } =
  compare_c_poly_simpl a1 b1 <? fun () ->
    compare_c_constructor_simpl a2 b2

let compare_output_break_ctor { a_k_var=a1; a_k'_var'=a2 } { a_k_var=b1; a_k'_var'=b2 } =
  compare_c_constructor_simpl a1 b1 <? fun () -> compare_c_constructor_simpl a2 b2

module OutputSpecialize1 : (Set.OrderedType with type t = output_specialize1) = struct
  type t = output_specialize1
  let compare = compare_output_specialize1
end


module BreakCtor : (Set.OrderedType with type t = output_break_ctor) = struct
  type t = output_break_ctor
  let compare = compare_output_break_ctor
end

let selector_specialize1 : (type_constraint_simpl, output_specialize1) selector =
  (* find two rules with the shape (a = forall b, d) and a = k'(var' …) or vice versa *)
  (* TODO: do the same for two rules with the shape (a = forall b, d) and tc(a…) *)
  (* TODO: do the appropriate thing for two rules with the shape (a = forall b, d) and (a = forall b', d') *)
  fun type_constraint_simpl dbs ->
  match type_constraint_simpl with
    SC_Constructor c                ->
    (* vice versa *)
    let other_cs = (UnionFindWrapper.get_constraints_related_to c.tv dbs).poly in
    let other_cs = List.filter (fun (x : c_poly_simpl) -> c.tv = x.tv) other_cs in (* TODO: does equality work in OCaml? *)
    let cs_pairs = List.map (fun x -> { poly = x ; a_k_var = c }) other_cs in
    WasSelected cs_pairs
  | SC_Alias       _                -> WasNotSelected (* TODO: ??? *)
  | SC_Poly        p                ->
    let other_cs = (UnionFindWrapper.get_constraints_related_to p.tv dbs).constructor in
    let other_cs = List.filter (fun (x : c_constructor_simpl) -> x.tv = p.tv) other_cs in (* TODO: does equality work in OCaml? *)
    let cs_pairs = List.map (fun x -> { poly = p ; a_k_var = x }) other_cs in
    WasSelected cs_pairs
  | SC_Typeclass   _                -> WasNotSelected

let propagator_specialize1 : output_specialize1 propagator =
  fun selected dbs ->
  let () = ignore (dbs) in (* this propagator doesn't need to use the dbs *)
  let a = selected.poly in
  let b = selected.a_k_var in
  let () = if (a.tv <> b.tv) then failwith "internal error" else () in

  (* produce constraints: *)

  (* create a fresh existential variable to instantiate the polymorphic type b *)
  let fresh_existential = Core.fresh_type_variable () in
  (* Produce the constraint (b.tv = a.body[a.binder |-> fresh_existential])
     The substitution is obtained by immediately applying the forall. *)
  let apply = (P_apply (P_forall a.forall , P_variable fresh_existential)) in
  let (reduced, new_constraints) = check_applied @@ type_level_eval apply in
  let eq1 = C_equation (P_variable b.tv, reduced) in
  let eqs = eq1 :: new_constraints in
  (eqs, []) (* no new assignments *)

module M (BlaBla : Set.OrderedType) = struct
  module AlreadySelected = Set.Make(BlaBla)

  let select_and_propagate : ('old_input, 'selector_output) selector -> BlaBla.t propagator -> _ -> 'a -> structured_dbs -> _ * new_constraints * new_assignments =
    fun selector propagator ->
    fun already_selected old_type_constraint dbs ->
      (* TODO: thread some state to know which selector outputs were already seen *)
      match selector old_type_constraint dbs with
        WasSelected selected_outputs ->
        (* TODO: fold instead. *)
        let (already_selected , selected_outputs) = List.fold_left (fun (already_selected, selected_outputs) elt -> if AlreadySelected.mem elt already_selected then (AlreadySelected.add elt already_selected , elt :: selected_outputs)
  else (already_selected , selected_outputs)) (already_selected , selected_outputs) selected_outputs in
        (* Call the propagation rule *)
        let new_contraints_and_assignments = List.map (fun s -> propagator s dbs) selected_outputs in
        let (new_constraints , new_assignments) = List.split new_contraints_and_assignments in
        (* return so that the new constraints are pushed to some kind of work queue and the new assignments stored *)
        (already_selected , List.flatten new_constraints , List.flatten new_assignments)
      | WasNotSelected ->
        (already_selected, [] , [])
end

module M_break_ctor = M(BreakCtor)
module M_specialize1 = M(OutputSpecialize1)

let select_and_propagate_break_ctor = M_break_ctor.select_and_propagate selector_break_ctor propagator_break_ctor
let select_and_propagate_specialize1 = M_specialize1.select_and_propagate selector_specialize1 propagator_specialize1

type already_selected = {
  break_ctor  : M_break_ctor.AlreadySelected.t  ;
  specialize1 : M_specialize1.AlreadySelected.t ;
}

(* Takes a constraint, applies all selector+propagator pairs to it.
   Keeps track of which constraints have already been selected. *)
let select_and_propagate_all' : _ -> type_constraint_simpl selector_input -> structured_dbs -> _ * new_constraints * structured_dbs =
  let aux sel_propag new_constraint (already_selected , new_constraints , dbs) =
    let (already_selected , new_constraints', new_assignments) = sel_propag already_selected new_constraint dbs in
    let assignments = List.fold_left (fun acc ({tv;c_tag=_;tv_list=_} as ele) -> TypeVariableMap.update tv (function None -> Some ele | x -> x) acc) dbs.assignments new_assignments in
    let dbs = { dbs with assignments } in
    (already_selected , new_constraints' @ new_constraints , dbs)
  in
  fun already_selected new_constraint dbs ->
    (* The order in which the propagators are applied to constraints
       is entirely accidental (dfs/bfs/something in-between). *)
    let (already_selected , new_constraints , dbs) = (already_selected , [] , dbs) in

    (* We must have a different already_selected for each selector,
       so this is more verbose than a few uses of `aux'. *)
    let (already_selected' , new_constraints , dbs) = aux select_and_propagate_break_ctor new_constraint (already_selected.break_ctor , new_constraints , dbs) in
    let (already_selected , new_constraints , dbs) = ({already_selected with break_ctor = already_selected'}, new_constraints , dbs) in

    let (already_selected' , new_constraints , dbs) = aux select_and_propagate_specialize1 new_constraint (already_selected.specialize1 , new_constraints , dbs) in
    let (already_selected , new_constraints , dbs) = ({already_selected with specialize1 = already_selected'}, new_constraints , dbs) in

    (already_selected , new_constraints , dbs)

(* Takes a list of constraints, applies all selector+propagator pairs
   to each in turn. *)
let rec select_and_propagate_all : _ -> type_constraint selector_input list -> structured_dbs -> _ * structured_dbs =
  fun already_selected new_constraints dbs ->
    match new_constraints with
    | [] -> (already_selected, dbs)
    | new_constraint :: tl ->
      let { state = dbs ; list = modified_constraints } = normalizers new_constraint dbs in
      let (already_selected , new_constraints' , dbs) =
        List.fold_left
          (fun (already_selected , nc , dbs) c ->
             let (already_selected , new_constraints' , dbs) = select_and_propagate_all' already_selected c dbs in
             (already_selected , new_constraints' @ nc , dbs))
          (already_selected , [] , dbs)
          modified_constraints in
      let new_constraints = new_constraints' @ tl in
      select_and_propagate_all already_selected new_constraints dbs

(* sub-component: constraint selector (worklist / dynamic queries) *)

(* constraint propagation: (buch of constraints) → (new constraints * assignments) *)





(* Below is a draft *)

(* type state = {
 *   (\* when α-renaming x to y, we put them in the same union-find class *\)
 *   unification_vars : unionfind ;
 *
 *   (\* assigns a value to the representant in the unionfind *\)
 *   assignments : type_expression TypeVariableMap.t ;
 *
 *   (\* constraints related to a type variable *\)
 *   constraints : constraints TypeVariableMap.t ;
 * } *)

type state = {
  structured_dbs   : structured_dbs   ;
  already_selected : already_selected ;
}

let initial_state : state = (* {
 *   unification_vars = UF.empty ;
 *   constraints = TypeVariableMap.empty ;
 *   assignments = TypeVariableMap.empty ;
 * } *)
{
  structured_dbs =
  {
    all_constraints = [] ; (* type_constraint_simpl list *)
    aliases = UF.empty ; (* unionfind *)
    assignments = TypeVariableMap.empty; (* c_constructor_simpl TypeVariableMap.t *)
    grouped_by_variable = TypeVariableMap.empty; (* constraints TypeVariableMap.t *)
    cycle_detection_toposort = (); (* unit *)
  } ;
  already_selected = {
    break_ctor = M_break_ctor.AlreadySelected.empty ;
    specialize1 = M_specialize1.AlreadySelected.empty ;
  }
}

(* This function is called when a program is fully compiled, and the
   typechecker's state is discarded. TODO: either get rid of the state
   earlier, or perform a sanity check here (e.g. that types have been
   inferred for all bindings and expressions, etc.

   Also, we should check at these places that we indeed do not need the
   state any further. Suzanne *)
let discard_state (_ : state) = ()

(* let replace_var_in_state = fun (v : type_variable) (state : state) -> *)
(*   let aux_tv : type_expression -> _ = function *)
(*     | P_forall    (w  , cs , tval) -> failwith "TODO" *)
(*     | P_variable  (w)              -> *)
(*       if w = v then *)
(*       (*…*) *)
(*       else *)
(*       (*…*) *)
(*     | P_constant     (c  , args)      -> failwith "TODO" *)
(*     | P_access_label (tv , label)     -> failwith "TODO" in *)
(*   let aux_tc tc = *)
(*     List.map (fun l -> List.map aux_tv l) tc in *)
(*   let aux : type_constraint -> _ = function *)
(*     | C_equation  (l , r)          -> C_equation  (aux_tv l , aux_tv r) *)
(*     | C_typeclass (l , rs)         -> C_typeclass (List.map aux_tv l , aux_tc rs) *)
(*   in List.map aux state *)

(* This is the solver *)
let aggregate_constraints : state -> type_constraint list -> state result = fun state newc ->
  (* TODO: Iterate over constraints *)
  let _todo = ignore (state, newc) in
  let (a, b) = select_and_propagate_all state.already_selected newc state.structured_dbs in
  ok { already_selected = a ; structured_dbs = b }
(*let { constraints ; eqv } = state in
  ok { constraints = constraints @ newc ; eqv }*)







(* Later on, we'll ensure that all the heuristics register the
   existential/unification variables that they create, as well as the
   new constraints that they create. We will then check that they only
   use a small set of core axioms to derive new constraints, and
   produce traces justifying that instanciations satisfy all related
   constraints, and that all existential variables are instantiated
   (possibly by first generalizing the type and then using the
   polymorphic type argument to instantiate the existential). *)

let placeholder_for_state_of_new_typer () = initial_state
