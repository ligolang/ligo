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
      SC_Constructor ({tv ; c_tag = _ ; tv_list} as c) -> store_constraint (tv :: tv_list)             {constructor = [c] ; poly = []  ; tc = [] ; row = []}
    | SC_Row         ({tv ; r_tag = _ ; tv_map } as c) -> store_constraint (tv :: LMap.to_list tv_map) {constructor = []  ; poly = []  ; tc = [] ; row = [c]}
    | SC_Typeclass   ({tc = _ ; args}            as c) -> store_constraint args                        {constructor = []  ; poly = []  ; tc = [c]; row = []}
    | SC_Poly        ({tv; forall = _}           as c) -> store_constraint [tv]                        {constructor = []  ; poly = [c] ; tc = [] ; row = []}
    | SC_Alias { a; b } -> Constraint_databases.merge_constraints a b dbs
  in (dbs , [new_constraint])

(** Stores the first assignment ('a = ctor('b, …)) that is encountered. -> why ?

    Subsequent ('a = ctor('b2, …)) with the same 'a are ignored. *)

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
 *)
let rec normalizer_simpl : (type_constraint , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
  (dbs, type_constraint_simpl new_constraint)

and type_constraint_simpl : type_constraint -> type_constraint_simpl list =
  fun new_constraint ->
  let insert_fresh a b =
    let fresh = Core.fresh_type_variable () in
    let cs1 = type_constraint_simpl (c_equation { tsrc = "solver: normalizer: simpl 1" ; t = P_variable fresh } a "normalizer: simpl 1") in
    let cs2 = type_constraint_simpl (c_equation { tsrc = "solver: normalizer: simpl 2" ; t = P_variable fresh } b "normalizer: simpl 2") in
    cs1 @ cs2 in
  let split_constant a c_tag args =
    let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map (fun (v,t) -> c_equation { tsrc = "solver: normalizer: split_constant" ; t = P_variable v } t "normalizer: split_constant") (List.combine fresh_vars args) in
    let recur = List.map type_constraint_simpl fresh_eqns in
    [SC_Constructor {tv=a;c_tag;tv_list=fresh_vars;reason_constr_simpl=Format.asprintf "normalizer: split constant %a = %a (%a)" Var.pp a Ast_typed.PP_generic.constant_tag c_tag (PP_helpers.list_sep Ast_typed.PP_generic.type_value (fun ppf () -> Format.fprintf ppf ", ")) args}] @ List.flatten recur in
  let split_row a r_tag args = 
    let aux const _ v =
      let var = Core.fresh_type_variable () in
      let v   = c_equation { tsrc = "solver: normalizer: split_row" ; t = P_variable var } v "normalizer: split_row" in
      (v::const, var)
    in
    let fresh_eqns, fresh_vars = LMap.fold_map aux [] args in
    let recur = List.map type_constraint_simpl fresh_eqns in
    [SC_Row {tv=a;r_tag;tv_map=fresh_vars;reason_row_simpl=Format.asprintf "normalizer: split constant %a = %a (%a)" Var.pp a Ast_typed.PP_generic.row_tag r_tag (Ast_typed.PP.record_sep Ast_typed.PP_generic.type_value (fun ppf () -> Format.fprintf ppf ", ")) args}] @ List.flatten recur in
  let gather_forall a forall = [SC_Poly { tv=a; forall ; reason_poly_simpl="normalizer: gather_forall"}] in
  let gather_alias a b = [SC_Alias { a ; b ; reason_alias_simpl="normalizer: gather_alias"}] in
  let reduce_type_app a b =
    let (reduced, new_constraints) = Typelang.check_applied @@ Typelang.type_level_eval b in
    let recur = List.map type_constraint_simpl new_constraints in
    let resimpl = type_constraint_simpl (c_equation a reduced "normalizer: reduce_type_app") in (* Note: this calls recursively but cant't fall in the same case. *)
    resimpl @ List.flatten recur in
  let split_typeclass args tc =
    let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map (fun (v,t) -> c_equation { tsrc = "solver: normalizer: split typeclass" ; t = P_variable v} t "normalizer: split_typeclass") (List.combine fresh_vars args) in
    let recur = List.map type_constraint_simpl fresh_eqns in
    [SC_Typeclass { tc ; args = fresh_vars ; reason_typeclass_simpl="normalizer: split_typeclass"}] @ List.flatten recur in

  match new_constraint.c with
  (* break down (forall 'b, body = forall 'c, body') into ('a = forall 'b, body and 'a = forall 'c, body')) *)
  | C_equation {aval=({ tsrc = _ ; t = P_forall _ } as a); bval=({ tsrc = _ ; t = P_forall _ } as b)}     -> insert_fresh a b
  (* break down (forall 'b, body = c(args)) into ('a = forall 'b, body and 'a = c(args)) *)
  | C_equation {aval=({ tsrc = _ ; t = P_forall _ } as a); bval=({ tsrc = _ ; t = P_constant _ } as b)}   -> insert_fresh a b
  (* break down (forall 'b, body = r(args)) into ('a = forall 'b, body and 'a = r(args)) *)
  | C_equation {aval=({ tsrc = _ ; t = P_forall _ } as a); bval=({ tsrc = _ ; t = P_row _ } as b)}   -> insert_fresh a b
  (* break down (c(args) = c'(args')) into ('a = c(args) and 'a = c'(args')) *)
  | C_equation {aval=({ tsrc = _ ; t = P_constant _ } as a); bval=({ tsrc = _ ; t = P_constant _ } as b)} -> insert_fresh a b
  (* break down (r(args) = r'(args')) into ('a = r(args) and 'a = r'(args')) *)
  | C_equation {aval=({ tsrc = _ ; t = P_row _ } as a); bval=({ tsrc = _ ; t = P_row _ } as b)} -> insert_fresh a b
  (* break down (c(args) = forall 'b, body) into ('a = c(args) and 'a = forall 'b, body) *)
  | C_equation {aval=({ tsrc = _ ; t = P_constant _ } as a); bval=({ tsrc = _ ; t = P_forall _ } as b)}   -> insert_fresh a b
  (* break down (r(args) = forall 'b, body) into ('a = r(args) and 'a = forall 'b, body) *)
  | C_equation {aval=({ tsrc = _ ; t = P_row _ } as a); bval=({ tsrc = _ ; t = P_forall _ } as b)}   -> insert_fresh a b
  (* break down (r(args) = c(args')) into ('a = r(args) and 'a = c(args')) *)
  | C_equation {aval=({ tsrc = _ ; t = P_constant _} as a); bval = ({ tsrc = _ ; t = P_row _} as b)}
  | C_equation {aval=({ tsrc = _ ; t = P_row _} as a); bval = ({ tsrc = _ ; t = P_constant _} as b)} -> insert_fresh a b
  | C_equation {aval={ tsrc = _ ; t = P_forall forall }; bval={ tsrc = _ ; t = P_variable b }}        -> gather_forall b forall
  | C_equation {aval={ tsrc = _ ; t = P_variable a }; bval={ tsrc = _ ; t = P_forall forall }}            -> gather_forall a forall
  | C_equation {aval={ tsrc = _ ; t = P_variable a }; bval={ tsrc = _ ; t = P_variable b }}               -> gather_alias a b
  | C_equation {aval={ tsrc = _ ; t = P_variable a }; bval={ tsrc = _ ; t = P_constant { p_ctor_tag; p_ctor_args } }}
  | C_equation {aval={ tsrc = _ ; t = P_constant {p_ctor_tag; p_ctor_args} }; bval={ tsrc = _ ; t = P_variable a }}   -> split_constant a p_ctor_tag p_ctor_args
  | C_equation {aval={ tsrc = _ ; t = P_variable a }; bval={ tsrc = _ ; t = P_row { p_row_tag; p_row_args } }}
  | C_equation {aval={ tsrc = _ ; t = P_row { p_row_tag; p_row_args }}; bval={ tsrc = _ ; t = P_variable a}} -> split_row a p_row_tag p_row_args
  (*  Reduce the type-level application, and simplify the resulting constraint + the extra constraints (typeclasses) that appeared at the forall binding site *)
  | C_equation {aval=(_ as a); bval=({ tsrc = _ ; t = P_apply _ } as b)}               -> reduce_type_app a b
  | C_equation {aval=({ tsrc = _ ; t = P_apply _ } as a); bval=(_ as b)}               -> reduce_type_app b a
  (* break down (TC(args)) into (TC('a, …) and ('a = arg) …) *)
  | C_typeclass { tc_args; typeclass }                              -> split_typeclass tc_args typeclass
  | C_access_label { c_access_label_tval; accessor; c_access_label_tvar } -> let _todo = ignore (c_access_label_tval, accessor, c_access_label_tvar) in failwith "TODO C_access_label" (* tv, label, result *)

let normalizers : type_constraint -> structured_dbs -> (structured_dbs , 'modified_constraint) state_list_monad =
  fun new_constraint dbs ->
    (fun x -> x)
    @@ lift normalizer_grouped_by_variable
    @@ lift normalizer_assignments
    @@ lift normalizer_all_constraints
    @@ lift normalizer_simpl
    @@ lift_state_list_monad ~state:dbs ~list:[new_constraint]
