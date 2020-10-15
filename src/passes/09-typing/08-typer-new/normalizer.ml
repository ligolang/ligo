module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
open Ast_typed.Misc
open Ast_typed.Types
open Typesystem.Solver_types
open Trace

(* sub-sub component: constraint normalizer: remove dupes and give structure
 * right now: union-find of unification vars
 * later: better database-like organisation of knowledge *)

(* Each normalizer returns an updated database (after storing the
   incoming constraint) and a list of constraints, used when the
   normalizer rewrites the constraints e.g. into simpler ones. *)
(* TODO: If implemented in a language with decent sets, should be 'b set not 'b list. *)
type ('a , 'b) normalizer = structured_dbs -> 'a -> (structured_dbs * 'b list)
type 'a normalizer_rm = structured_dbs -> 'a -> (structured_dbs, Typer_common.Errors.typer_error) result
(* type ('a , 'b) normalizer = {
 *   add: ('a, 'b) normalizer_add;
 *   rm: 'a normalizer_rm;
 * } *)

(* TODO: make this be threaded around (not essential, but we tend to
   avoid mutable stuff). *)
let global_next_constraint_id : int64 ref = ref 0L

(** Updates the dbs.all_constraints field when new constraints are
   discovered.

   This field contains a list of all the constraints, without any form of
   grouping or sorting. *)
let normalizer_all_constraints : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
    ({ dbs with all_constraints = new_constraint :: dbs.all_constraints } , [new_constraint])

let normalizer_all_constraints_remove : type_constraint_simpl normalizer_rm =
  fun dbs constraint_to_rm ->
  (* TODO: use a set, not a list. *)
  (* TODO: compare type constraints by their constrain id instead of by equality. *)
  (* TODO: proper failure if the element doesn't exist (don't catch the exception as the comparator may throw a similar exception on its own). *)
  let all_constraints = List.remove_element ~compare:Ast_typed.Compare.type_constraint_simpl constraint_to_rm dbs.all_constraints in
  ok { dbs with all_constraints }

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

let normalizer_grouped_by_variable_remove : type_constraint_simpl normalizer_rm =
  (* for each type variable affected by the constraint as specified by the normalizers,
     remove that constraint from the grouped_by_variable *)
  fun dbs constraint_to_rm ->
  let rm_constraint tvars constraints =
    let aux dbs (tvar : type_variable) =
      Constraint_databases.rm_constraints_related_to tvar constraints dbs
    in bind_fold_list aux dbs tvars
  in
  match constraint_to_rm with
      SC_Constructor ({tv ; c_tag = _ ; tv_list} as c) -> rm_constraint (tv :: tv_list)             {constructor = [c] ; poly = []  ; tc = [] ; row = []}
    | SC_Row         ({tv ; r_tag = _ ; tv_map } as c) -> rm_constraint (tv :: LMap.to_list tv_map) {constructor = []  ; poly = []  ; tc = [] ; row = [c]}
    | SC_Typeclass   ({tc = _ ; args}            as c) -> rm_constraint args                        {constructor = []  ; poly = []  ; tc = [c]; row = []}
    | SC_Poly        ({tv; forall = _}           as c) -> rm_constraint [tv]                        {constructor = []  ; poly = [c] ; tc = [] ; row = []}
    | SC_Alias { a; b } -> ignore (a,b); fail (Typer_common.Errors.internal_error __LOC__ "can't remove aliasing constraints")
      (* Constraint_databases.merge_constraints a b dbs *)
  

let normalizer_by_constraint_identifier : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
  let dbs = match new_constraint with
    | SC_Typeclass c -> Constraint_databases.register_by_constraint_identifier c dbs
    | _ -> dbs
  in (dbs , [new_constraint])

let normalizer_by_constraint_identifier_remove : type_constraint_simpl normalizer_rm =
  fun dbs constraint_to_rm ->
  let%bind by_constraint_identifier = match constraint_to_rm with
    | Ast_typed.Types.SC_Typeclass { id_typeclass_simpl; _ } ->
      (* TODO: a proper error instead of an exception *)
      ok @@ Map.remove id_typeclass_simpl dbs.by_constraint_identifier
    | _ -> ok dbs.by_constraint_identifier in
  ok { dbs with by_constraint_identifier }


let normalizer_refined_typeclasses : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
  match new_constraint with
  | SC_Typeclass c ->
    (* dbs.refined_typeclasses : map id → id *)

    (* stores a copy of the typeclass constraint c if there was no
       existing refined_typeclass for it *)
    let open Heuristic_tc_fundep_utils in
    let tc = tc_to_constraint_identifier c in
    (match PolyMap.find_opt tc dbs.refined_typeclasses with
       None ->
       let copied = {
         c with
         is_mandatory_constraint = false;
         id_typeclass_simpl = ConstraintIdentifier (!global_next_constraint_id);
       } in
       let metadata = make_refined_typeclass copied ~original:c in
       let copied' = SC_Typeclass copied in
       let dbs = {
         dbs with
         refined_typeclasses = PolyMap.update
             tc
             (function
                 Some _existing ->
                 failwith "Internal error: attempted to register two refined typeclasses for the same typeclass"
               | None -> Some metadata)
             dbs.refined_typeclasses;
         refined_typeclasses_back = PolyMap.update
             metadata.refined
             (function
                 Some _existing -> 
                 failwith "Internal error: ???"
               | None -> Some tc)
             dbs.refined_typeclasses_back
       } in
       dbs, [copied'; new_constraint]
     | Some _ -> dbs, [new_constraint])
  | _ -> dbs, [new_constraint]

let normalizer_refined_typeclasses_remove : type_constraint_simpl normalizer_rm =
  fun dbs constraint_to_rm ->
  match constraint_to_rm with
  | SC_Typeclass c ->
    let original =
      try
        PolyMap.find c dbs.refined_typeclasses_back
      with
        Not_found ->
        failwith "Internal error: Can't remove refined typeclass: it is not attached to a typeclass."
    in
    ok @@ {
      dbs with
      refined_typeclasses =
        PolyMap.remove original dbs.refined_typeclasses;
      refined_typeclasses_back =
        PolyMap.remove c dbs.refined_typeclasses_back
    }
  | _ -> ok dbs

let normalizer_typeclasses_constrained_by : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
  let dbs = match new_constraint with
    | SC_Typeclass c -> Constraint_databases.register_typeclasses_constrained_by c dbs
    | _ -> dbs
  in (dbs , [new_constraint])

let constraint_identifier_to_tc (dbs : structured_dbs) (ci : constraint_identifier) =
  (* TODO: this can fail: catch the exception and throw an error *)
  RedBlackTrees.PolyMap.find ci dbs.by_constraint_identifier 
let tc_to_constraint_identifier : c_typeclass_simpl -> constraint_identifier =
  fun tc -> tc.id_typeclass_simpl

let normalizer_typeclasses_constrained_by_remove : type_constraint_simpl normalizer_rm =
  fun dbs c ->
  match c with
  | Ast_typed.Types.SC_Typeclass c ->
    let tc = tc_to_constraint_identifier c in
    let aux' = function
        Some set -> Some (Set.remove tc set)
      | None -> Some (Set.remove tc (Set.create ~cmp:Ast_typed.Compare.constraint_identifier)) in
    let aux typeclasses_constrained_by tv =
      Map.update tv aux' typeclasses_constrained_by in
    let typeclasses_constrained_by =
      List.fold_left
        aux
        dbs.typeclasses_constrained_by
        (List.rev (constraint_identifier_to_tc dbs tc).args) in
    ok { dbs with typeclasses_constrained_by }
  | _ -> ok dbs


(** Stores the first assignment ('a = ctor('b, …)) that is encountered
    (all assignments should use compatible types).

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
    [SC_Constructor { is_mandatory_constraint = true; tv=a;c_tag;tv_list=fresh_vars;reason_constr_simpl=Format.asprintf "normalizer: split constant %a = %a (%a)" Var.pp a Ast_typed.PP.constant_tag c_tag (PP_helpers.list_sep Ast_typed.PP.type_value (fun ppf () -> Format.fprintf ppf ", ")) args}] @ List.flatten recur in
  let split_row a r_tag args =
    let aux const _ v =
      let var = Core.fresh_type_variable () in
      let v   = c_equation { tsrc = "solver: normalizer: split_row" ; t = P_variable var } v "normalizer: split_row" in
      (v::const, var)
    in
    let fresh_eqns, fresh_vars = LMap.fold_map aux [] args in
    let recur = List.map type_constraint_simpl fresh_eqns in
    [SC_Row {is_mandatory_constraint = true; tv=a;r_tag;tv_map=fresh_vars;reason_row_simpl=Format.asprintf "normalizer: split constant %a = %a (%a)" Var.pp a Ast_typed.PP.row_tag r_tag (Ast_typed.PP.record_sep Ast_typed.PP.type_value (fun ppf () -> Format.fprintf ppf ", ")) args}] @ List.flatten recur in
  let gather_forall a forall = [SC_Poly { is_mandatory_constraint = true; tv=a; forall ; reason_poly_simpl="normalizer: gather_forall"}] in
  let gather_alias a b = [SC_Alias { is_mandatory_constraint = true; a ; b ; reason_alias_simpl="normalizer: gather_alias"}] in
  let reduce_type_app a b =
    let (reduced, new_constraints) = Typelang.check_applied @@ Typelang.type_level_eval b in
    let recur = List.map type_constraint_simpl new_constraints in
    let resimpl = type_constraint_simpl (c_equation a reduced "normalizer: reduce_type_app") in (* Note: this calls recursively but cant't fall in the same case. *)
    resimpl @ List.flatten recur in
  let split_typeclass args tc =
    let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map (fun (v,t) -> c_equation { tsrc = "solver: normalizer: split typeclass" ; t = P_variable v} t "normalizer: split_typeclass") (List.combine fresh_vars args) in
    let recur = List.map type_constraint_simpl fresh_eqns in
    let id_typeclass_simpl = ConstraintIdentifier (!global_next_constraint_id) in
    global_next_constraint_id := Int64.add !global_next_constraint_id 1L;
    [SC_Typeclass { is_mandatory_constraint = true; tc ; args = fresh_vars ; id_typeclass_simpl ; reason_typeclass_simpl="normalizer: split_typeclass"}] @ List.flatten recur in

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
    (* TODO: this does no exhaustiveness check to ensure that all parts of the database were updated as needed. *)
    @@ lift normalizer_refined_typeclasses
    @@ lift normalizer_typeclasses_constrained_by
    @@ lift normalizer_by_constraint_identifier
    @@ lift normalizer_grouped_by_variable
    @@ lift normalizer_assignments
    @@ lift normalizer_all_constraints
    @@ lift normalizer_simpl
    @@ lift_state_list_monad ~state:dbs ~list:[new_constraint]

let normalizers_remove : structured_dbs -> type_constraint_simpl -> (structured_dbs, _) result =
  let _ = bind_map_list in
  (* TODO: figure out what parts of the database are initial
     constraints (goals in Coq, can't be removed), what parts are
     outputs (assignments to evars in Coq, can't be removed), and what
     parts are temporary hypotheses (safe to remove). *)
  fun dbs constraint_to_remove ->
    let (>>?) (dc : (structured_dbs * type_constraint_simpl,  _) result) (f : type_constraint_simpl normalizer_rm) : (structured_dbs * type_constraint_simpl,  _) result = let%bind (d, c) = dc in let%bind d = f d c in ok (d, c) in
    let%bind (dbs, _) =
      ok (dbs, constraint_to_remove)
      (* TODO: this does no exhaustiveness check to ensure that all parts of the database were updated as needed. *)
      >>? normalizer_refined_typeclasses_remove
      >>? normalizer_typeclasses_constrained_by_remove
      >>? normalizer_by_constraint_identifier_remove
      >>? normalizer_grouped_by_variable_remove
      (* >>? lift normalizer_assignments_remove *)
      >>? normalizer_all_constraints_remove
      (* >>? lift normalizer_simpl *)
    in ok dbs
