(* selector / propagation rule for breaking down composite types
 * For now: break pair(a, b) = pair(c, d) into a = c, b = d *)

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

type selector_output = output_break_ctor

let selector_ : type_constraint_simpl -> type_variable GroupedByVariable.t -> selector_output list =
  (* find two rules with the shape x = k(var …) and x = k'(var' …) *)
  fun type_constraint_simpl grouped_by_variable_map ->
    match type_constraint_simpl with
    | SC_Constructor c -> (
    Format.printf "In break_ctor.selector_ for %a\n%!" Ast_typed.PP.type_constraint_simpl_short type_constraint_simpl;
      (* finding other constraints related to the same type variable and
      with the same sort of constraint (constructor vs. constructor)
      is symmetric *)
      let other_rows_lhs = GroupedByVariable.get_rows_by_lhs c.tv grouped_by_variable_map in
      let other_constructors_lhs = 
        List.filter (fun x -> not @@  (Ast_typed.Compare.c_constructor_simpl c x = 0)) @@ MultiSet.elements @@
        GroupedByVariable.get_constructors_by_lhs c.tv grouped_by_variable_map in
      Format.printf "Other constructor : (%a)\n%!" Ast_typed.PP.(list_sep_d c_constructor_simpl_short) other_constructors_lhs;
      let () = ( if MultiSet.is_empty other_rows_lhs
                 then ()
                 else failwith (Format.asprintf "TODO: type error with %a ; %a" Ast_typed.PP.c_constructor_simpl c (MultiSet.pp Ast_typed.PP.c_row_simpl) other_rows_lhs))
      in    
      let cs_pairs = List.map (fun x -> { a_k_var = `Constructor c ; a_k'_var' = `Constructor x }) other_constructors_lhs in
      cs_pairs
    )
    | SC_Alias       _                -> [] (* TODO: ??? (beware: symmetry) *)
    | SC_Typeclass   _                -> []
    | SC_Poly        _                -> [] (* TODO: ??? (beware: symmetry) *)
    | SC_Row         r                -> (
    Format.printf "In break_ctor.selector_ for %a\n%!" Ast_typed.PP.type_constraint_simpl_short type_constraint_simpl;
      let other_rows_lhs = 
        List.filter (fun x -> not @@  (Ast_typed.Compare.c_row_simpl r x = 0)) @@ MultiSet.elements @@
        GroupedByVariable.get_rows_by_lhs r.tv grouped_by_variable_map in
      let constructors_lhs = GroupedByVariable.get_constructors_by_lhs r.tv grouped_by_variable_map in
      let () = ( if MultiSet.is_empty constructors_lhs
                 then ()
                 else failwith (Format.asprintf "TODO: type error with %a ; %a" Ast_typed.PP.c_row_simpl r (MultiSet.pp Ast_typed.PP.c_constructor_simpl) constructors_lhs)) in
      let cs_pairs = List.map (fun x -> { a_k_var = `Row r ; a_k'_var' = `Row x }) other_rows_lhs in
      cs_pairs
    )

let selector : type_constraint_simpl -> _ flds -> selector_output list =
  fun type_constraint_simpl indexes -> selector_ type_constraint_simpl indexes#grouped_by_variable
(* when a = k(…) and b = k'(…) are in the db, aliasing a and b should
   check if they're non-empty (and in that case produce a
   selector_output for all pairs / more efficiently any single pair
   since the break_ctor creates equivalence classes for the
   constructor arguments) *)

let alias_selector : type_variable -> type_variable -> _ flds -> selector_output list =
  fun a b indexes ->
  Format.printf "Break_ctor.alias_selector %a %a\n%!" Ast_typed.PP.type_variable a Ast_typed.PP.type_variable b ;
  let a_constructors = GroupedByVariable.get_constructors_by_lhs a indexes#grouped_by_variable in
  let b_constructors = GroupedByVariable.get_constructors_by_lhs b indexes#grouped_by_variable in
  let a_rows = GroupedByVariable.get_rows_by_lhs a indexes#grouped_by_variable in
  let b_rows = GroupedByVariable.get_rows_by_lhs b indexes#grouped_by_variable in
  let a_ctor = MultiSet.map_elements (fun a -> `Constructor a) a_constructors in
  let b_ctor = MultiSet.map_elements (fun a -> `Constructor a) b_constructors in
  let a_row = List.map (fun a -> `Row a) (MultiSet.elements a_rows) in
  let b_row = List.map (fun a -> `Row a) (MultiSet.elements b_rows) in
  match a_ctor @ a_row with
  | [] -> []
  | old_ctors_hd :: _ ->
    (match b_ctor @ b_row with
       [] -> []
     | new_ctors_hd :: _ ->
       [{ a_k_var = old_ctors_hd ; a_k'_var' = new_ctors_hd }])

let propagator : (output_break_ctor, typer_error) propagator =
  fun selected repr ->
  let a = selected.a_k_var in
  let b = selected.a_k'_var' in
  let get_tv : constructor_or_row -> type_variable = fun cr ->
    match cr with
    | `Row r -> r.tv
    | `Constructor c -> c.tv
  in
  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  let a_tv = repr @@ get_tv a in
  let b_tv = repr @@ get_tv b in
  assert (Var.equal a_tv b_tv);
  (* produce constraints: *)
  (* a.tv = b.tv *) (* nope, already the same *)
  (* let eq1 = c_equation (wrap (Propagator_break_ctor "a") @@ P_variable a_tv) (wrap (Propagator_break_ctor "b") @@ P_variable b_tv) "propagator: break_ctor" in *)
  (* let () = if Ast_typed.Debug.debug_new_typer then
      let p = Ast_typed.PP.c_constructor_simpl in
      Printf.fprintf stderr "%s" @@ Format.asprintf "\npropagator_break_ctor\na = %a\nb = %a\n%!" p a p b in *)
  (* a.c_tag = b.c_tag *)
  ( match a , b with
    | `Row a , `Row b ->
      if (Solver_should_be_generated.compare_simple_c_row a.r_tag b.r_tag) <> 0 then
        (* TODO : use error monad *)
        failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)"
                    Solver_should_be_generated.debug_pp_c_row_simpl a
                    Solver_should_be_generated.debug_pp_c_row_simpl b
                    (Solver_should_be_generated.compare_simple_c_row a.r_tag b.r_tag))
    | `Constructor a , `Constructor b ->
      if (Solver_should_be_generated.compare_simple_c_constant a.c_tag b.c_tag) <> 0 then
        (* TODO : use error monad *)
        failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)"
                    Solver_should_be_generated.debug_pp_c_constructor_simpl a
                    Solver_should_be_generated.debug_pp_c_constructor_simpl b
                    (Solver_should_be_generated.compare_simple_c_constant a.c_tag b.c_tag))
    | _ -> failwith "type error"
  );
  (* Produce constraint a.tv_list = b.tv_list *)
  let%bind eqs3 =
    match a , b with
    | `Row a , `Row b ->
      let aux = fun ((la,aa),(lb,bb)) ->
        let%bind () = Trace.Assert.assert_true (corner_case "TODO: different labels la lb") (Ast_typed.Compare.label la lb = 0) in
        ok @@ c_equation
          (wrap (Propagator_break_ctor "a") @@ P_variable aa)
          (wrap (Propagator_break_ctor "b") @@ P_variable bb)
          "propagator: break_ctor: row"
      in
      let%bind bindings =  List.map2 (fun x y -> (x,y)) (LMap.bindings a.tv_map) (LMap.bindings b.tv_map)
        ~ok ~fail:(fun _ _-> fail @@ (corner_case "TODO: different number of labels (List.length a.tv_map) (List.length b.tv_map)"))
      in
      bind_map_list aux bindings
    | `Constructor a , `Constructor b -> (
      let aux = fun aa bb -> c_equation (wrap (Propagator_break_ctor "a") @@ P_variable aa) (wrap (Propagator_break_ctor "b") @@ P_variable bb) "propagator: break_ctor: ctor" in
      List.map2 aux a.tv_list b.tv_list
        ~ok ~fail:(fun _ _ -> fail @@ different_constant_tag_number_of_arguments __LOC__ a.c_tag b.c_tag (List.length a.tv_list) (List.length b.tv_list))
    )
    | _ -> failwith "type error"
  in
  let eqs = eqs3 in
  Format.printf "Break_ctor : returning with new constraint %a\n%!" (PP_helpers.list_sep_d Ast_typed.PP.type_constraint_short) @@ eqs ;
  ok [
    {
      remove_constraints = [];
      add_constraints = eqs;
      proof_trace = Axiom Axioms.f_equal
    }
  ]

let printer = Ast_typed.PP.output_break_ctor
let printer_json = Ast_typed.Yojson.output_break_ctor
let comparator = Solver_should_be_generated.compare_output_break_ctor

let heuristic = Heuristic_plugin { heuristic_name = "break_ctor"; selector; alias_selector; propagator; printer; printer_json; comparator }
