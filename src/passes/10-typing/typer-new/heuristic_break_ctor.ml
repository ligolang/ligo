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
      (* finding other constraints related to the same type variable and
      with the same sort of constraint (constructor vs. constructor)
      is symmetric *)
      let constraints_lhs = GroupedByVariable.get_constraints_by_lhs c.tv grouped_by_variable_map in
      let () = ( match constraints_lhs.row with
        | hd::_ -> failwith (Format.asprintf "TODO: type error with %a ; %a" Ast_typed.PP.c_constructor_simpl c Ast_typed.PP.c_row_simpl hd)
        | _ -> () )
      in    
      let other_cs = constraints_lhs.constructor in
      let cs_pairs = List.map (fun x -> { a_k_var = `Constructor c ; a_k'_var' = `Constructor x }) other_cs in
      cs_pairs
    )
    | SC_Alias       _                -> [] (* TODO: ??? (beware: symmetry) *)
    | SC_Typeclass   _                -> []
    | SC_Poly        _                -> [] (* TODO: ??? (beware: symmetry) *)
    | SC_Row         r                -> (
      let constraints_lhs = GroupedByVariable.get_constraints_by_lhs r.tv grouped_by_variable_map in
      let () = ( match constraints_lhs.constructor with
      | hd::_ -> failwith (Format.asprintf "TODO: type error with %a ; %a" Ast_typed.PP.c_constructor_simpl hd Ast_typed.PP.c_row_simpl r)
      | _ -> () )
      in    
      let other_cs = constraints_lhs.row in
      let cs_pairs = List.map (fun x -> { a_k_var = `Row r ; a_k'_var' = `Row x }) other_cs in
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
  let a = GroupedByVariable.get_constraints_by_lhs a indexes#grouped_by_variable in
  let b = GroupedByVariable.get_constraints_by_lhs b indexes#grouped_by_variable in
  let a_ctor = List.map (fun a -> `Constructor a) a.constructor in
  let b_ctor = List.map (fun a -> `Constructor a) b.constructor in
  let a_row = List.map (fun a -> `Row a) a.row in
  let b_row = List.map (fun a -> `Row a) b.row in
  match a_ctor @ a_row with
  | [] -> []
  | old_ctors_hd :: _ ->
    (match b_ctor @ b_row with
       [] -> []
     | new_ctors_hd :: _ ->
       [{ a_k_var = old_ctors_hd ; a_k'_var' = new_ctors_hd }])

let propagator : (output_break_ctor, typer_error) propagator =
  fun selected ->
  let a = selected.a_k_var in
  let b = selected.a_k'_var' in
  let get_tv : constructor_or_row -> type_variable = fun cr ->
    match cr with
    | `Row r -> r.tv
    | `Constructor c -> c.tv
  in
  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  let a_tv = get_tv a in
  let b_tv = get_tv b in
  assert (Var.equal a_tv b_tv);
  (* produce constraints: *)
  (* a.tv = b.tv *)
  let eq1 = c_equation (wrap (Propagator_break_ctor "a") @@ P_variable a_tv) (wrap (Propagator_break_ctor "b") @@ P_variable b_tv) "propagator: break_ctor" in
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
          (wrap (Propagator_break_ctor "a") @@ P_variable bb)
          "propagator: break_ctor"
      in
      let%bind bindings =  List.map2 (fun x y -> (x,y)) (LMap.bindings a.tv_map) (LMap.bindings b.tv_map)
        ~ok ~fail:(fun _ _-> fail @@ (corner_case "TODO: different number of labels (List.length a.tv_map) (List.length b.tv_map)"))
      in
      bind_map_list aux bindings
    | `Constructor a , `Constructor b -> (
      let aux = fun aa bb -> c_equation (wrap (Propagator_break_ctor "a") @@ P_variable aa) (wrap (Propagator_break_ctor "a") @@ P_variable bb) "propagator: break_ctor" in
      List.map2 aux a.tv_list b.tv_list
        ~ok ~fail:(fun _ _ -> fail @@ different_constant_tag_number_of_arguments __LOC__ a.c_tag b.c_tag (List.length a.tv_list) (List.length b.tv_list))
    )
    | _ -> failwith "type error"
  in
  let eqs = eq1 :: eqs3 in
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

let heuristic = Heuristic_plugin { selector; alias_selector; propagator; printer; printer_json; comparator }
