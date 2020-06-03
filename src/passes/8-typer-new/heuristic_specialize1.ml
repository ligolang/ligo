(* selector / propagation rule for specializing polymorphic types
 * For now: (x = forall y, z) and (x = k'(var' …))
 * produces the new constraint (z[x |-> k'(var' …)])
 * where [from |-> to] denotes substitution. *)

module Core = Typesystem.Core
open Ast_typed.Misc
open Ast_typed.Types
open Typesystem.Solver_types

let selector : (type_constraint_simpl, output_specialize1) selector =
  (* find two rules with the shape (x = forall b, d) and x = k'(var' …) or vice versa *)
  (* TODO: do the same for two rules with the shape (a = forall b, d) and tc(a…) *)
  (* TODO: do the appropriate thing for two rules with the shape (a = forall b, d) and (a = forall b', d') *)
  fun type_constraint_simpl dbs ->
  match type_constraint_simpl with
    SC_Constructor c                ->
    (* vice versa *)
    let other_cs = (Constraint_databases.get_constraints_related_to c.tv dbs).poly in
    let other_cs = List.filter (fun (x : c_poly_simpl) -> Var.equal c.tv x.tv) other_cs in
    let cs_pairs = List.map (fun x -> { poly = x ; a_k_var = c }) other_cs in
    WasSelected cs_pairs
  | SC_Alias       _                -> WasNotSelected (* TODO: ??? *)
  | SC_Poly        p                ->
    let other_cs = (Constraint_databases.get_constraints_related_to p.tv dbs).constructor in
    let other_cs = List.filter (fun (x : c_constructor_simpl) -> Var.equal x.tv p.tv) other_cs in
    let cs_pairs = List.map (fun x -> { poly = p ; a_k_var = x }) other_cs in
    WasSelected cs_pairs
  | SC_Typeclass   _                -> WasNotSelected

let propagator : output_specialize1 propagator =
  fun dbs selected ->
  let () = ignore (dbs) in (* this propagator doesn't need to use the dbs *)
  let a = selected.poly in
  let b = selected.a_k_var in

  (* The selector is expected to provide two constraints with the shape (x = forall y, z) and x = k'(var' …) *)
  assert (Var.equal (a : c_poly_simpl).tv (b : c_constructor_simpl).tv);

  (* produce constraints: *)

  (* create a fresh existential variable to instantiate the polymorphic type y *)
  let fresh_existential = Core.fresh_type_variable () in
  (* Produce the constraint (b.tv = a.body[a.binder |-> fresh_existential])
     The substitution is obtained by immediately applying the forall. *)
  let apply = {
      tsrc = "solver: propagator: specialize1 apply" ;
      t = P_apply { tf   = { tsrc = "solver: propagator: specialize1 tf"   ; t = P_forall a.forall            };
                    targ = { tsrc = "solver: propagator: specialize1 targ" ; t = P_variable fresh_existential }} } in
  let (reduced, new_constraints) = Typelang.check_applied @@ Typelang.type_level_eval apply in
  let eq1 = c_equation { tsrc = "solver: propagator: specialize1 eq1" ; t = P_variable b.tv } reduced "propagator: specialize1" in
  let eqs = eq1 :: new_constraints in
  (eqs, []) (* no new assignments *)

let heuristic = Propagator_heuristic { selector ; propagator ; comparator = Solver_should_be_generated.compare_output_specialize1 }
