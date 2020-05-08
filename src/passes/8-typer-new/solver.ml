open Trace

module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2

module Wrap = Wrap
open Wrap
open Ast_typed.Misc

(* TODO: remove this, it's not used anymore *)
module TypeVariable =
struct
  type t = Core.type_variable
  let compare a b = Var.compare a b
  let to_string = (fun s -> Format.asprintf "%a" Var.pp s)

end



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

open Ast_typed.Types

module UnionFindWrapper = struct
  (* Light wrapper for API for grouped_by_variable in the structured
     db, to access it modulo unification variable aliases. *)
  let get_constraints_related_to : type_variable -> structured_dbs -> constraints =
    fun variable dbs ->
      let variable , aliases = UF.get_or_set variable dbs.aliases in
      let dbs = { dbs with aliases } in
      match Map.find_opt variable dbs.grouped_by_variable with
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
      let grouped_by_variable = Map.update variable_repr (function
            None -> Some c
          | Some (x : constraints) -> Some {
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
        match Map.find_opt ab dbs.grouped_by_variable with
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
        Map.add variable_repr_a all_constraints dbs.grouped_by_variable in
      let dbs = { dbs with grouped_by_variable} in
      let grouped_by_variable =
        Map.remove variable_repr_b dbs.grouped_by_variable in
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
    | SC_Alias { a; b } -> UnionFindWrapper.merge_constraints a b dbs
  in (dbs , [new_constraint])

(** Stores the first assinment ('a = ctor('b, …)) that is encountered.

    Subsequent ('a = ctor('b2, …)) with the same 'a are ignored.

    TOOD: are we checking somewhere that 'b … = 'b2 … ? *)
let normalizer_assignments : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
    match new_constraint with
    | SC_Constructor ({tv ; c_tag = _ ; tv_list = _} as c) ->
      let assignments = Map.update tv (function None -> Some c | e -> e) dbs.assignments in
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
      { tsrc = _ ; t = P_apply _ } -> failwith "internal error: shouldn't happen" (* failwith "could not reduce type-level application. Arbitrary type-level applications are not supported for now." *)
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
    let (dbs , cs1) = normalizer_simpl dbs (c_equation { tsrc = "solver: normalizer: simpl 1" ; t = P_variable fresh } a "normalizer: simpl 1") in
    let (dbs , cs2) = normalizer_simpl dbs (c_equation { tsrc = "solver: normalizer: simpl 2" ; t = P_variable fresh } b "normalizer: simpl 2") in
    (dbs , cs1 @ cs2) in
  let split_constant a c_tag args =
    let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map (fun (v,t) -> c_equation { tsrc = "solver: normalizer: split_constant" ; t = P_variable v } t "normalizer: split_constant") (List.combine fresh_vars args) in
    let (dbs , recur) = List.fold_map_acc normalizer_simpl dbs fresh_eqns in
    (dbs , [SC_Constructor {tv=a;c_tag;tv_list=fresh_vars;reason_constr_simpl=Format.asprintf "normalizer: split constant %a = %a (%a)" Var.pp a Ast_typed.PP_generic.constant_tag c_tag (PP_helpers.list_sep Ast_typed.PP_generic.type_value (fun ppf () -> Format.fprintf ppf ", ")) args}] @ List.flatten recur) in
  let gather_forall a forall = (dbs , [SC_Poly { tv=a; forall ; reason_poly_simpl="normalizer: gather_forall"}]) in
  let gather_alias a b = (dbs , [SC_Alias { a ; b ; reason_alias_simpl="normalizer: gather_alias"}]) in
  let reduce_type_app a b =
    let (reduced, new_constraints) = check_applied @@ type_level_eval b in
    let (dbs , recur) = List.fold_map_acc normalizer_simpl dbs new_constraints in
    let (dbs , resimpl) = normalizer_simpl dbs (c_equation a reduced "normalizer: reduce_type_app") in (* Note: this calls recursively but cant't fall in the same case. *)
    (dbs , resimpl @ List.flatten recur) in
  let split_typeclass args tc =
    let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map (fun (v,t) -> c_equation { tsrc = "solver: normalizer: split typeclass" ; t = P_variable v} t "normalizer: split_typeclass") (List.combine fresh_vars args) in
    let (dbs , recur) = List.fold_map_acc normalizer_simpl dbs fresh_eqns in
    (dbs, [SC_Typeclass { tc ; args = fresh_vars ; reason_typeclass_simpl="normalizer: split_typeclass"}] @ List.flatten recur) in

  match new_constraint.c with
  (* break down (forall 'b, body = forall 'c, body') into ('a = forall 'b, body and 'a = forall 'c, body')) *)
  | C_equation {aval=({ tsrc = _ ; t = P_forall _ } as a); bval=({ tsrc = _ ; t = P_forall _ } as b)}     -> insert_fresh a b
  (* break down (forall 'b, body = c(args)) into ('a = forall 'b, body and 'a = c(args)) *)
  | C_equation {aval=({ tsrc = _ ; t = P_forall _ } as a); bval=({ tsrc = _ ; t = P_constant _ } as b)}   -> insert_fresh a b
  (* break down (c(args) = c'(args')) into ('a = c(args) and 'a = c'(args')) *)
  | C_equation {aval=({ tsrc = _ ; t = P_constant _ } as a); bval=({ tsrc = _ ; t = P_constant _ } as b)} -> insert_fresh a b
  (* break down (c(args) = forall 'b, body) into ('a = c(args) and 'a = forall 'b, body) *)
  | C_equation {aval=({ tsrc = _ ; t = P_constant _ } as a); bval=({ tsrc = _ ; t = P_forall _ } as b)}   -> insert_fresh a b
  | C_equation {aval={ tsrc = _ ; t = P_forall forall }; bval={ tsrc = _ ; t = P_variable b }}        -> gather_forall b forall
  | C_equation {aval={ tsrc = _ ; t = P_variable a }; bval={ tsrc = _ ; t = P_forall forall }}            -> gather_forall a forall
  | C_equation {aval={ tsrc = _ ; t = P_variable a }; bval={ tsrc = _ ; t = P_variable b }}               -> gather_alias a b
  | C_equation {aval={ tsrc = _ ; t = P_variable a }; bval={ tsrc = _ ; t = P_constant { p_ctor_tag; p_ctor_args } }} -> split_constant a p_ctor_tag p_ctor_args
  | C_equation {aval={ tsrc = _ ; t = P_constant {p_ctor_tag; p_ctor_args} }; bval={ tsrc = _ ; t = P_variable b }}   -> split_constant b p_ctor_tag p_ctor_args
  (*  Reduce the type-level application, and simplify the resulting constraint + the extra constraints (typeclasses) that appeared at the forall binding site *)
  | C_equation {aval=(_ as a); bval=({ tsrc = _ ; t = P_apply _ } as b)}               -> reduce_type_app a b
  | C_equation {aval=({ tsrc = _ ; t = P_apply _ } as a); bval=(_ as b)}               -> reduce_type_app b a
  (* break down (TC(args)) into (TC('a, …) and ('a = arg) …) *)
  | C_typeclass { tc_args; typeclass }                              -> split_typeclass tc_args typeclass
  | C_access_label { c_access_label_tval; accessor; c_access_label_tvar } -> let _todo = ignore (c_access_label_tval, accessor, c_access_label_tvar) in failwith "TODO" (* tv, label, result *)

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

let selector_break_ctor :  (type_constraint_simpl, output_break_ctor) selector =
  (* find two rules with the shape x = k(var …) and x = k'(var' …) *)
  fun type_constraint_simpl dbs ->
  match type_constraint_simpl with
    SC_Constructor c ->
    (* finding other constraints related to the same type variable and
       with the same sort of constraint (constructor vs. constructor)
       is symmetric *)
    let other_cs = (UnionFindWrapper.get_constraints_related_to c.tv dbs).constructor in
    let other_cs = List.filter (fun (o : c_constructor_simpl) -> Var.equal c.tv o.tv) other_cs in
    (* TODO double-check the conditions in the propagator, we had a
       bug here because the selector was too permissive. *)
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
      | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_option -> (function
      | C_arrow -> 1
      | C_option -> 0
      | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_record -> (function
      | C_arrow | C_option  -> 1
      | C_record -> 0
      | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_variant -> (function
      | C_arrow | C_option  | C_record -> 1
      | C_variant -> 0
      | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_map -> (function
      | C_arrow | C_option  | C_record | C_variant -> 1
      | C_map -> 0
      | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_big_map -> (function
      | C_arrow | C_option  | C_record | C_variant | C_map -> 1
      | C_big_map -> 0
      | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_list -> (function
      | C_arrow | C_option  | C_record | C_variant | C_map | C_big_map -> 1
      | C_list -> 0
      | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_set -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list -> 1
      | C_set -> 0
      | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_unit -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set -> 1
      | C_unit -> 0
       | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_string -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  -> 1
      | C_string -> 0
      | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_nat -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string -> 1
      | C_nat -> 0
      | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_mutez -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat -> 1
      | C_mutez -> 0
      | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_timestamp -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez -> 1
      | C_timestamp -> 0
      | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_int -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp -> 1
      | C_int -> 0
      | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_address -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int -> 1
      | C_address -> 0
      | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_bytes -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address -> 1
      | C_bytes -> 0
      | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_key_hash -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes -> 1
      | C_key_hash -> 0
      | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_key -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash -> 1
      | C_key -> 0
      | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_signature -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key -> 1
      | C_signature -> 0
      | C_operation | C_contract | C_chain_id -> -1)
  | C_operation -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature -> 1
      | C_operation -> 0
      | C_contract | C_chain_id -> -1)
  | C_contract -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation -> 1
      | C_contract -> 0
      | C_chain_id -> -1)
  | C_chain_id -> (function
      | C_arrow | C_option | C_record | C_variant | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract -> 1
      | C_chain_id -> 0
      (* N/A -> -1 *)
    )

(* Using a pretty-printer from the PP.ml module creates a dependency
   loop, so the one that we need temporarily for debugging purposes
   has been copied here. *)
let debug_pp_constant : _ -> constant_tag -> unit = fun ppf c_tag ->
    let ct = match c_tag with
      | T.C_arrow     -> "arrow"
      | T.C_option    -> "option"
      | T.C_record    -> failwith "record"
      | T.C_variant   -> failwith "variant"
      | T.C_map       -> "map"
      | T.C_big_map   -> "big_map"
      | T.C_list      -> "list"
      | T.C_set       -> "set"
      | T.C_unit      -> "unit"
      | T.C_string    -> "string"
      | T.C_nat       -> "nat"
      | T.C_mutez     -> "mutez"
      | T.C_timestamp -> "timestamp"
      | T.C_int       -> "int"
      | T.C_address   -> "address"
      | T.C_bytes     -> "bytes"
      | T.C_key_hash  -> "key_hash"
      | T.C_key       -> "key"
      | T.C_signature -> "signature"
      | T.C_operation -> "operation"
      | T.C_contract  -> "contract"
      | T.C_chain_id  -> "chain_id"
    in
    Format.fprintf ppf "%s" ct

let debug_pp_c_constructor_simpl ppf { tv; c_tag; tv_list } =
  Format.fprintf ppf "CTOR %a %a(%a)" Var.pp tv debug_pp_constant c_tag PP_helpers.(list_sep Var.pp (const " , ")) tv_list

let propagator_break_ctor : output_break_ctor propagator =
  fun selected dbs ->
  let () = ignore (dbs) in (* this propagator doesn't need to use the dbs *)
  let a = selected.a_k_var in
  let b = selected.a_k'_var' in

  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  assert (Var.equal (a : c_constructor_simpl).tv (b : c_constructor_simpl).tv);

  (* produce constraints: *)

  (* a.tv = b.tv *)
  let eq1 = c_equation { tsrc = "solver: propagator: break_ctor a" ; t = P_variable a.tv} { tsrc = "solver: propagator: break_ctor b" ; t = P_variable b.tv} "propagator: break_ctor" in
  (* a.c_tag = b.c_tag *)
  if (compare_simple_c_constant a.c_tag b.c_tag) <> 0 then
    failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)" debug_pp_c_constructor_simpl a debug_pp_c_constructor_simpl b (compare_simple_c_constant a.c_tag b.c_tag))
  else
    (* a.tv_list = b.tv_list *)
  if List.length a.tv_list <> List.length b.tv_list then
    failwith "type error: incompatible types, not same length"
  else
    let eqs3 = List.map2 (fun aa bb -> c_equation { tsrc = "solver: propagator: break_ctor aa" ; t = P_variable aa} { tsrc = "solver: propagator: break_ctor bb" ; t = P_variable bb} "propagator: break_ctor") a.tv_list b.tv_list in
    let eqs = eq1 :: eqs3 in
    (eqs , []) (* no new assignments *)

(* TODO : with our selectors, the selection depends on the order in which the constraints are added :-( :-( :-( :-(
   We need to return a lazy stream of constraints. *)



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
let compare_label (a:label) (b:label) = 
  let Label a = a in
  let Label b = b in
  String.compare a b
let rec compare_typeclass a b = compare_list (compare_list compare_type_expression) a b
and compare_type_expression { tsrc = _ ; t = ta } { tsrc = _ ; t = tb } =
  (* Note: this comparison ignores the tsrc, the idea is that types
     will often be compared to see if they are the same, regardless of
     where the type comes from .*)
  compare_type_expression_ ta tb
and compare_type_expression_ = function
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
  | P_constant { p_ctor_tag=a1; p_ctor_args=a2 } -> (function
      | P_forall _ -> 1
      | P_variable _ -> 1
      | P_constant { p_ctor_tag=b1; p_ctor_args=b2 } -> compare_simple_c_constant a1 b1 <? fun () -> compare_list compare_type_expression a2 b2
      | P_apply _ -> -1)
  | P_apply { tf=a1; targ=a2 } -> (function
      | P_forall _ -> 1
      | P_variable _ -> 1
      | P_constant _ -> 1
      | P_apply { tf=b1; targ=b2 } -> compare_type_expression a1 b1 <? fun () -> compare_type_expression a2 b2)
and compare_type_constraint = fun { c = ca ; reason = ra } { c = cb ; reason = rb } ->
  let c = compare_type_constraint_ ca cb in
  if c < 0 then -1
  else if c = 0 then String.compare ra rb
  else 1
and compare_type_constraint_ = function
  | C_equation { aval=a1; bval=a2 } -> (function
      | C_equation { aval=b1; bval=b2 } -> compare_type_expression a1 b1 <? fun () -> compare_type_expression a2 b2
      | C_typeclass _ -> -1
      | C_access_label _ -> -1)
  | C_typeclass { tc_args=a1; typeclass=a2 } -> (function
      | C_equation _ -> 1
      | C_typeclass { tc_args=b1; typeclass=b2 } -> compare_list compare_type_expression a1 b1 <? fun () -> compare_typeclass a2 b2
      | C_access_label _ -> -1)
  | C_access_label { c_access_label_tval=a1; accessor=a2; c_access_label_tvar=a3 } -> (function
      | C_equation _ -> 1
      | C_typeclass _ -> 1
      | C_access_label { c_access_label_tval=b1; accessor=b2; c_access_label_tvar=b3 } -> compare_type_expression a1 b1 <? fun () -> compare_label a2 b2  <? fun () -> compare_type_variable a3 b3)
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
let compare_c_constructor_simpl { reason_constr_simpl = _ ; tv=a1; c_tag=a2; tv_list=a3 } { reason_constr_simpl = _ ; tv=b1; c_tag=b2; tv_list=b3 } =
  (* We do not compare the reasons, as they are only for debugging and
     not part of the type *)
  compare_type_variable a1 b1 <? fun () -> compare_simple_c_constant a2 b2  <? fun () -> compare_list compare_type_variable a3 b3

let compare_output_specialize1 { poly = a1; a_k_var = a2 } { poly = b1; a_k_var = b2 } =
  compare_c_poly_simpl a1 b1 <? fun () ->
    compare_c_constructor_simpl a2 b2

let compare_output_break_ctor { a_k_var=a1; a_k'_var'=a2 } { a_k_var=b1; a_k'_var'=b2 } =
  compare_c_constructor_simpl a1 b1 <? fun () -> compare_c_constructor_simpl a2 b2

let selector_specialize1 : (type_constraint_simpl, output_specialize1) selector =
  (* find two rules with the shape (x = forall b, d) and x = k'(var' …) or vice versa *)
  (* TODO: do the same for two rules with the shape (a = forall b, d) and tc(a…) *)
  (* TODO: do the appropriate thing for two rules with the shape (a = forall b, d) and (a = forall b', d') *)
  fun type_constraint_simpl dbs ->
  match type_constraint_simpl with
    SC_Constructor c                ->
    (* vice versa *)
    let other_cs = (UnionFindWrapper.get_constraints_related_to c.tv dbs).poly in
    let other_cs = List.filter (fun (x : c_poly_simpl) -> Var.equal c.tv x.tv) other_cs in
    let cs_pairs = List.map (fun x -> { poly = x ; a_k_var = c }) other_cs in
    WasSelected cs_pairs
  | SC_Alias       _                -> WasNotSelected (* TODO: ??? *)
  | SC_Poly        p                ->
    let other_cs = (UnionFindWrapper.get_constraints_related_to p.tv dbs).constructor in
    let other_cs = List.filter (fun (x : c_constructor_simpl) -> Var.equal x.tv p.tv) other_cs in
    let cs_pairs = List.map (fun x -> { poly = p ; a_k_var = x }) other_cs in
    WasSelected cs_pairs
  | SC_Typeclass   _                -> WasNotSelected

let propagator_specialize1 : output_specialize1 propagator =
  fun selected dbs ->
  let () = ignore (dbs) in (* this propagator doesn't need to use the dbs *)
  let a = selected.poly in
  let b = selected.a_k_var in

  (* The selector is expected to provice two constraints with the shape (x = forall y, z) and x = k'(var' …) *)
  assert (Var.equal (a : c_poly_simpl).tv (b : c_constructor_simpl).tv);

  (* produce constraints: *)

  (* create a fresh existential variable to instantiate the polymorphic type y *)
  let fresh_existential = Core.fresh_type_variable () in
  (* Produce the constraint (b.tv = a.body[a.binder |-> fresh_existential])
     The substitution is obtained by immediately applying the forall. *)
  let apply = { tsrc = "solver: propagator: specialize1 apply" ; t = P_apply {tf = { tsrc = "solver: propagator: specialize1 tf" ; t = P_forall a.forall }; targ = { tsrc = "solver: propagator: specialize1 targ" ; t = P_variable fresh_existential }} } in
  let (reduced, new_constraints) = check_applied @@ type_level_eval apply in
  let eq1 = c_equation { tsrc = "solver: propagator: specialize1 eq1" ; t = P_variable b.tv } reduced "propagator: specialize1" in
  let eqs = eq1 :: new_constraints in
  (eqs, []) (* no new assignments *)

let select_and_propagate : ('old_input, 'selector_output) selector -> _ propagator -> _ -> 'a -> structured_dbs -> _ * new_constraints * new_assignments =
  fun selector propagator ->
  fun already_selected old_type_constraint dbs ->
  (* TODO: thread some state to know which selector outputs were already seen *)
  match selector old_type_constraint dbs with
    WasSelected selected_outputs ->
     let open RedBlackTrees.PolySet in
     let { set = already_selected ; duplicates = _ ; added = selected_outputs } = add_list selected_outputs already_selected in
     (* Call the propagation rule *)
     let new_contraints_and_assignments = List.map (fun s -> propagator s dbs) selected_outputs in
     let (new_constraints , new_assignments) = List.split new_contraints_and_assignments in
     (* return so that the new constraints are pushed to some kind of work queue and the new assignments stored *)
     (already_selected , List.flatten new_constraints , List.flatten new_assignments)
  | WasNotSelected ->
     (already_selected, [] , [])

let select_and_propagate_break_ctor = select_and_propagate selector_break_ctor propagator_break_ctor
let select_and_propagate_specialize1 = select_and_propagate selector_specialize1 propagator_specialize1

(* Takes a constraint, applies all selector+propagator pairs to it.
   Keeps track of which constraints have already been selected. *)
let select_and_propagate_all' : _ -> type_constraint_simpl selector_input -> structured_dbs -> _ * new_constraints * structured_dbs =
  let aux sel_propag new_constraint (already_selected , new_constraints , dbs) =
    let (already_selected , new_constraints', new_assignments) = sel_propag already_selected new_constraint dbs in
    let assignments = List.fold_left (fun acc ({tv;c_tag=_;tv_list=_} as ele) -> Map.update tv (function None -> Some ele | x -> x) acc) dbs.assignments new_assignments in
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

let initial_state : typer_state = (* {
 *   unification_vars = UF.empty ;
 *   constraints = TypeVariableMap.empty ;
 *   assignments = TypeVariableMap.empty ;
 * } *)
{
  structured_dbs =
  {
    all_constraints = [] ; (* type_constraint_simpl list *)
    aliases = UF.empty (fun s -> Format.asprintf "%a" Var.pp s) Var.compare ; (* unionfind *)
    assignments = Map.create ~cmp:Var.compare; (* c_constructor_simpl TypeVariableMap.t *)
    grouped_by_variable = Map.create ~cmp:Var.compare; (* constraints TypeVariableMap.t *)
    cycle_detection_toposort = (); (* unit *)
  } ;
  already_selected = {
    break_ctor = Set.create ~cmp:compare_output_break_ctor;
    specialize1 = Set.create ~cmp:compare_output_specialize1 ;
  }
}

(* This function is called when a program is fully compiled, and the
   typechecker's state is discarded. TODO: either get rid of the state
   earlier, or perform a sanity check here (e.g. that types have been
   inferred for all bindings and expressions, etc.

   Also, we should check at these places that we indeed do not need the
   state any further. Suzanne *)
let discard_state (_ : typer_state) = ()

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
let aggregate_constraints : typer_state -> type_constraint list -> typer_state result = fun state newc ->
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
