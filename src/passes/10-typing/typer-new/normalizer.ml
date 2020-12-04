(* module Core = Typesystem.Core
 * module Map = RedBlackTrees.PolyMap
 * open Ast_typed.Misc
 * open Ast_typed.Types
 * open Typesystem.Solver_types
 * open Trace
 * 
 * (\* sub-sub component: constraint normalizer: remove dupes and give structure
 *  * right now: union-find of unification vars
 *  * later: better database-like organisation of knowledge *\)
 * 
 * (\* Each normalizer returns an updated database (after storing the
 *    incoming constraint) and a list of constraints, used when the
 *    normalizer rewrites the constraints e.g. into simpler ones. *\)
 * (\* TODO: If implemented in a language with decent sets, should be 'b set not 'b list. *\)
 * type ('a , 'b) normalizer = structured_dbs -> 'a -> (structured_dbs * 'b list)
 * type 'a normalizer_rm = structured_dbs -> 'a -> (structured_dbs, Typer_common.Errors.typer_error) result
 * 
 * (\* type ('a , 'b) normalizer = {
 *  *   add: ('a, 'b) normalizer_add;
 *  *   rm: 'a normalizer_rm;
 *  * } *\)
 * 
 * (\* TODO: make this be threaded around (not essential, but we tend to
 *    avoid mutable stuff). *\)
 * let global_next_constraint_id : int64 ref = ref 0L
 * 
 * (\** Updates the dbs.all_constraints field when new constraints are
 *    discovered.
 * 
 *    This field contains a list of all the constraints, without any form of
 *    grouping or sorting. *\)
 * let normalizer_all_constraints : (type_constraint_simpl , type_constraint_simpl) normalizer =
 *   fun dbs new_constraint ->
 *     ({ dbs with all_constraints = new_constraint :: dbs.all_constraints } , [new_constraint])
 * 
 * let normalizer_all_constraints_remove : type_constraint_simpl normalizer_rm =
 *   fun dbs constraint_to_rm ->
 *   (\* TODO: use a set, not a list. *\)
 *   (\* TODO: compare type constraints by their constrain id instead of by equality. *\)
 *   (\* TODO: proper failure if the element doesn't exist (don't catch the exception as the comparator may throw a similar exception on its own). *\)
 *   let all_constraints = List.remove_element ~compare:Ast_typed.Compare.type_constraint_simpl constraint_to_rm dbs.all_constraints in
 *   ok { dbs with all_constraints }
 * 
 * let constraint_identifier_to_tc (dbs : structured_dbs) (ci : constraint_identifier) =
 *   (\* TODO: this can fail: catch the exception and throw an error *\)
 *   RedBlackTrees.PolyMap.find ci dbs.by_constraint_identifier 
 * let tc_to_constraint_identifier : c_typeclass_simpl -> constraint_identifier =
 *   fun tc -> tc.id_typeclass_simpl
 * 
 * 
 * 
 * (\** Stores the first assignment ('a = ctor('b, …)) that is encountered
 *     (all assignments should use compatible types).
 * 
 *     Subsequent ('a = ctor('b2, …)) with the same 'a are ignored. *\)
 * 
 * let normalizer_assignments : (type_constraint_simpl , type_constraint_simpl) normalizer =
 *   fun dbs new_constraint ->
 *     match new_constraint with
 *     | SC_Constructor ({tv ; c_tag = _ ; tv_list = _} as c) ->
 *       let assignments = Map.update tv (function None -> Some c | e -> e) dbs.assignments in
 *       let dbs = {dbs with assignments} in
 *       (dbs , [new_constraint])
 *     | _ ->
 *       (dbs , [new_constraint])
 * 
 * (\* TODO: at some point there may be uses of named type aliases (type
 *    foo = int; let x : foo = 42). These should be inlined. *\)
 * let normalizer_simpl : (type_constraint , type_constraint_simpl) normalizer =
 *   fun dbs new_constraint ->
 *   (dbs, type_constraint_simpl new_constraint)
 * 
 * let normalizers : type_constraint -> structured_dbs -> (structured_dbs , 'modified_constraint) state_list_monad =
 *   fun new_constraint dbs ->
 *     (fun x -> x)
 *     (\* TODO: this does no exhaustiveness check to ensure that all parts of the database were updated as needed. *\)
 * 
 *     (\* Below are normalizers which store constraints into some sort of database to enable fast lookups. *\)
 *     @@ lift normalizer_refined_typeclasses
 *     @@ lift normalizer_typeclasses_constrained_by
 *     @@ lift normalizer_by_constraint_identifier
 *     @@ lift normalizer_grouped_by_variable
 *     @@ lift normalizer_assignments
 *     @@ lift normalizer_all_constraints
 *     (\* below are a different sort of normalizers, they break down constraints into more elementrary ones *\)
 *     @@ lift normalizer_simpl
 *     @@ lift_state_list_monad ~state:dbs ~list:[new_constraint]
 * 
 * let normalizers_remove : structured_dbs -> type_constraint_simpl -> (structured_dbs, _) result =
 *   let _ = bind_map_list in
 *   (\* TODO: figure out what parts of the database are initial
 *      constraints (goals in Coq, can't be removed), what parts are
 *      outputs (assignments to evars in Coq, can't be removed), and what
 *      parts are temporary hypotheses (safe to remove). *\)
 *   fun dbs constraint_to_remove ->
 *     let (>>?) (dc : (structured_dbs * type_constraint_simpl,  _) result) (f : type_constraint_simpl normalizer_rm) : (structured_dbs * type_constraint_simpl,  _) result = let%bind (d, c) = dc in let%bind d = f d c in ok (d, c) in
 *     let%bind (dbs, _) =
 *       ok (dbs, constraint_to_remove)
 *       (\* TODO: this does no exhaustiveness check to ensure that all parts of the database were updated as needed. *\)
 *       >>? normalizer_refined_typeclasses_remove
 *       >>? normalizer_typeclasses_constrained_by_remove
 *       >>? normalizer_by_constraint_identifier_remove
 *       >>? normalizer_grouped_by_variable_remove
 *       (\* >>? lift normalizer_assignments_remove *\)
 *       >>? normalizer_all_constraints_remove
 *       (\* >>? lift normalizer_simpl *\)
 *     in ok dbs *)
