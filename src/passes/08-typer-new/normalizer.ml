module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
open Ast_typed.Misc
open Ast_typed.Types
open Typesystem.Solver_types

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
      Constraint_databases.add_constraints_related_to tvar constraints dbs
    in List.fold_left aux dbs tvars
  in
  let dbs = match new_constraint with
      SC_Constructor ({tv ; c_tag = _ ; tv_list} as c) -> store_constraint (tv :: tv_list) {constructor = [c] ; poly = []  ; tc = []}
    | SC_Typeclass   ({tc = _ ; args}            as c) -> store_constraint args            {constructor = []  ; poly = []  ; tc = [c]}
    | SC_Poly        ({tv; forall = _}           as c) -> store_constraint [tv]            {constructor = []  ; poly = [c] ; tc = []}
    | SC_Alias { a; b } -> Constraint_databases.merge_constraints a b dbs
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
    let (reduced, new_constraints) = Typelang.check_applied @@ Typelang.type_level_eval b in
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

let normalizers : type_constraint -> structured_dbs -> (structured_dbs , 'modified_constraint) state_list_monad =
  fun new_constraint dbs ->
    (fun x -> x)
    @@ lift normalizer_grouped_by_variable
    @@ lift normalizer_assignments
    @@ lift normalizer_all_constraints
    @@ lift normalizer_simpl
    @@ lift_state_list_monad ~state:dbs ~list:[new_constraint]
