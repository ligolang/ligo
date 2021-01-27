(* selector / propagation rule for specializing polymorphic types
 * For now: (x = forall y, z) and (x = k'(var' …))
 * produces the new constraint (x = z[y ↦ fresh_y])
 * where [from ↦ to] denotes substitution. *)

module Core = Typesystem.Core
open Ast_typed.Misc
open Ast_typed.Types
open Typesystem.Solver_types
open Trace
open Typer_common.Errors
open Database_plugins.All_plugins
open Ast_typed.Reasons

type 'a flds = <
  grouped_by_variable : type_variable GroupedByVariable.t ;
  ..
> as 'a

type selector_output = output_specialize1

(* TODO: we need to detect if a ∀ constraint has already been specialized or not
   The same need was present for the heuristic_tc_fundep (detect if a TC has already
   been refined, and if so find the update) *)
 
 let selector : type_constraint_simpl -> _ flds -> selector_output list =
  (* find two rules with the shape (x = forall b, d) and x = k'(var' …) or vice versa *)
  (* TODO: do the same for two rules with the shape (a = forall b, d) and tc(a…) *)
  (* TODO: do the appropriate thing for two rules with the shape (a = forall b, d) and (a = forall b', d') *)
  fun type_constraint_simpl indexes ->
  match type_constraint_simpl with
  | SC_Constructor c                ->
    (* vice versa *)
    let other_cs = MultiSet.elements @@ GroupedByVariable.get_polys_by_lhs c.tv indexes#grouped_by_variable in
    let cs_pairs = List.map (fun x -> { poly = x ; a_k_var = c }) other_cs in
    cs_pairs
  | SC_Alias       _                -> failwith "alias should not be visible here"
  | SC_Poly        p                ->
    let other_cs = MultiSet.elements @@ GroupedByVariable.get_constructors_by_lhs p.tv indexes#grouped_by_variable in
    let cs_pairs = List.map (fun x -> { poly = p ; a_k_var = x }) other_cs in
    cs_pairs
  | SC_Typeclass   _                -> []
  | SC_Row _                        -> []

(* when α = ∀ δ, γ and β = κ(ε, …) are in the db, aliasing α and β
   should check they are non-empty (and in that case produce a
   selector_output for all pairs) *)

let alias_selector : type_variable -> type_variable -> _ flds -> selector_output list =
  fun a b indexes ->
  let a_polys = MultiSet.elements @@ GroupedByVariable.get_polys_by_lhs a indexes#grouped_by_variable in
  let a_ctors = MultiSet.elements @@ GroupedByVariable.get_constructors_by_lhs a indexes#grouped_by_variable in
  let b_polys = MultiSet.elements @@ GroupedByVariable.get_polys_by_lhs b indexes#grouped_by_variable in
  let b_ctors = MultiSet.elements @@ GroupedByVariable.get_constructors_by_lhs b indexes#grouped_by_variable in
  List.flatten @@
  List.map
    (fun poly ->
       List.map
         (fun ctor ->
            { poly ; a_k_var = ctor })
         (a_ctors @ b_ctors))
    (a_polys @ b_polys)

let propagator : (output_specialize1 , typer_error) propagator =
  fun selected _repr ->
  let a = selected.poly in
  let b = selected.a_k_var in

  (* The selector is expected to provide two constraints with the shape (x = forall y, z) and x = k'(var' …) *)
  assert (Var.equal (a : c_poly_simpl).tv (b : c_constructor_simpl).tv);

  (* produce constraints: *)

  (* create a fresh existential variable to instantiate the polymorphic type y *)
  let fresh_existential = Core.fresh_type_variable () in
  (* Produce the constraint (b.tv = a.body[a.binder |-> fresh_existential])
     The substitution is obtained by immediately applying the forall. *)
  (* TODO: this should not use apply, universally-quantified types are *not* type-level functions, even though the substitution is identical on both. *)
  let apply =
    wrap Propagator_specialize_apply @@
      P_apply { tf   = wrap Propagator_specialize_tf @@ P_forall a.forall ;
                targ = wrap Propagator_specialize_targ @@ P_variable fresh_existential ;
      }
  in
  let (reduced, new_constraints) = Typelang.check_applied @@ Typelang.type_level_eval apply in
  (if Ast_typed.Debug.debug_new_typer
   then Printf.fprintf stderr "%s"
     @@ Format.asprintf "apply = %a\nb = %a\nreduced = %a\nnew_constraints = [\n%a\n]\n"
       Ast_typed.PP.type_value apply
       Ast_typed.PP.c_constructor_simpl b
       Ast_typed.PP.type_value reduced
       (PP_helpers.list_sep Ast_typed.PP.type_constraint_short (fun ppf () -> Format.fprintf ppf " ;\n")) new_constraints);
  
  let eq1 = c_equation (wrap (Todo "solver: propagator: specialize1 eq1") @@ P_variable b.tv) reduced "propagator: specialize1" in
  let eqs = eq1 :: new_constraints in
  let pp_indented_constraint_list =
    let open PP_helpers in
    let open Ast_typed.PP in
    (list_sep type_constraint_short (tag "\n  ")) in
  let () = Format.printf "specialize1: rm %a, add:\n  %a\n\n%!" Ast_typed.PP.type_constraint_simpl_short (SC_Poly a) pp_indented_constraint_list eqs in
  Format.printf "Specialize : returning with new constraint %a\n%!" (PP_helpers.list_sep_d Ast_typed.PP.type_constraint_short) @@ eqs ;
    ok [
        {
          remove_constraints = [ SC_Poly a ];
          add_constraints = eqs;
          proof_trace = Axiom Axioms.specialize
        }
      ]

let printer = Ast_typed.PP.output_specialize1
let printer_json = Ast_typed.Yojson.output_specialize1
let comparator = Solver_should_be_generated.compare_output_specialize1

let heuristic = Heuristic_plugin { heuristic_name = "specialize1"; selector; alias_selector; propagator; printer; printer_json; comparator }
