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

type selector_output = {
  a_k_var : c_row_simpl ;
  a_var_l : c_access_label_simpl ;
}

let printer ppf {a_k_var;a_var_l} =
  Format.fprintf ppf "{@[<hv 2> @ a_k_var : %a;@ a_var_l : %a;@]@ }"
    Ast_typed.PP.c_row_simpl a_k_var
    Ast_typed.PP.c_access_label_simpl a_var_l

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> _ flds -> selector_output list =
  (* find two rules with the shape x = k(var …) and x = k'(var' …) *)
  fun repr type_constraint_simpl indexes ->
  (* Format.printf "In access_label.selector for %a and indeces %a\n%!" Ast_typed.PP.type_constraint_simpl_short type_constraint_simpl (GroupedByVariable.pp Ast_typed.PP.type_variable) indexes#grouped_by_variable; *)
  match type_constraint_simpl with
  | SC_Constructor c -> (
      let other_access_labels_record_types = GroupedByVariable.get_access_labels_by_record_type (repr c.tv) indexes#grouped_by_variable in
      if MultiSet.is_empty other_access_labels_record_types then
        []
      else
        failwith (Format.asprintf "TODO: type error with %a ; %a" Ast_typed.PP.c_constructor_simpl c (MultiSet.pp Ast_typed.PP.c_access_label_simpl) other_access_labels_record_types)
    )
  | SC_Alias       _                -> []
  | SC_Typeclass   _                -> []
  | SC_Access_label l               -> (
      let other_rows_lhs = GroupedByVariable.get_rows_by_lhs (repr l.record_type) indexes#grouped_by_variable in
      let other_constructors_lhs = GroupedByVariable.get_constructors_by_lhs (repr l.record_type) indexes#grouped_by_variable in
      let other_records_lhs, other_variants_lhs = List.partition (function { r_tag = C_record; _ } -> true | { r_tag = C_variant; _ } -> false) (MultiSet.elements other_rows_lhs) in
      if List.length other_variants_lhs != 0 then
        failwith (Format.asprintf "TODO: type error with %a (needs a record, but) %a (are variants)" Ast_typed.PP.c_access_label_simpl l (Ast_typed.PP.list_sep_d Ast_typed.PP.c_row_simpl) other_variants_lhs)
      else if not (MultiSet.is_empty other_constructors_lhs) then
        failwith (Format.asprintf "TODO: type error with %a (needs a record, but) %a (are constructors)" Ast_typed.PP.c_access_label_simpl l (MultiSet.pp Ast_typed.PP.c_constructor_simpl) other_constructors_lhs)
      else
      let cs_pairs = List.map (fun x -> { a_k_var = x ; a_var_l = l }) other_records_lhs in
      (* Format.printf "cs_pairs (%a)\n%!" (PP_helpers.list_sep_d printer) cs_pairs; *)
      cs_pairs
    )
  | SC_Poly        _                -> []
  | SC_Row         ({ r_tag = C_record ; _ } as r) -> (
      let other_access_labels_lhs = GroupedByVariable.get_access_labels_by_record_type (repr r.tv) indexes#grouped_by_variable in
      let cs_pairs = MultiSet.map_elements (fun x -> { a_k_var = r ; a_var_l = x }) other_access_labels_lhs in
      (* Format.printf "cs_pairs (%a)\n%!" (PP_helpers.list_sep_d printer) cs_pairs; *)
      cs_pairs
    )
  | SC_Row         _ -> []

let alias_selector : type_variable -> type_variable -> _ flds -> selector_output list =
  fun a b indexes ->
  (* Format.printf "In access_label.alias_selector %a %a\n%!" Ast_typed.PP.type_variable a Ast_typed.PP.type_variable b ; *)
  let a_access_labels = GroupedByVariable.get_access_labels_by_record_type a indexes#grouped_by_variable in
  let b_access_labels = GroupedByVariable.get_access_labels_by_record_type b indexes#grouped_by_variable in
  let a_rows = GroupedByVariable.get_rows_by_lhs a indexes#grouped_by_variable in
  let b_rows = GroupedByVariable.get_rows_by_lhs b indexes#grouped_by_variable in
  (* let a_ctor = MultiSet.map_elements (fun a -> `Constructor a) a_constructors in
   * let b_ctor = MultiSet.map_elements (fun a -> `Constructor a) b_constructors in *)
  (* TODO: have a separate group of plug-ins which detect errors *)
  let a_records = List.filter_map (function { r_tag = C_record; _ } as x -> Some x | { r_tag = C_variant; _ } -> None) (MultiSet.elements a_rows) in
  let b_records = List.filter_map (function { r_tag = C_record; _ } as x -> Some x | { r_tag = C_variant; _ } -> None) (MultiSet.elements b_rows) in
  match a_records @ b_records with
  | [] -> []
  | old_records_hd :: _ ->
    List.map (fun al -> {a_k_var = old_records_hd; a_var_l = al}) (MultiSet.elements a_access_labels @ MultiSet.elements b_access_labels)

let get_referenced_constraints ({ a_k_var; a_var_l } : selector_output) : type_constraint_simpl list =
  [
    SC_Row a_k_var;
    SC_Access_label a_var_l;
  ]

let propagator : (selector_output, typer_error) propagator =
  fun selected repr ->
  (* Format.printf "In access_label.propagator for \n%!"; *)
  let a_var_l = selected.a_var_l in
  let a_k_var = selected.a_k_var in
  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  let row_tv = repr a_k_var.tv in
  let record_type = repr a_var_l.record_type in
  let access_result = repr a_var_l.tv in
  assert (Var.equal row_tv record_type);
  (* produce constraints: *)

  let%bind () = match a_k_var.r_tag with
    | C_record -> ok ()
    | C_variant -> fail @@ corner_case "Type error: can't access field on variant"
  in

  let%bind field_type =
    match LMap.find_opt a_var_l.label a_k_var.tv_map with
    | None -> fail @@ corner_case "Type error: label {a_var_l.label} does not exist in record {a_k_var.tv_map}"
    | Some field_type -> ok @@ repr field_type.associated_variable
      
  in

  (* Produce constraint a_k_var.tv_map[label] = a_var_l.tv *)
  let eqs = [
    c_equation
      (wrap (Propagator_break_ctor "a") @@ P_variable access_result)
      (wrap (Propagator_break_ctor "b") @@ P_variable field_type)
      "propagator: break_ctor: row"
  ] in
  
  ok [
    {
      remove_constraints = [];
      add_constraints = eqs;
      proof_trace = Axiom (HandWaved "TODO: proof trace") (* Axiom Axioms.f_equal *)
    }
  ]

let printer_json {a_k_var;a_var_l} =
  `Assoc [
    ("a_k_var", Ast_typed.Yojson.c_row_simpl a_k_var);
    ("a_var_l", Ast_typed.Yojson.c_access_label_simpl a_var_l)]
let comparator { a_k_var=a1; a_var_l=a2 } { a_k_var=b1; a_var_l=b2 } =
  let open Solver_should_be_generated in
  compare_c_row_simpl a1 b1 <? fun () -> compare_c_access_label_simpl a2 b2

let heuristic = Heuristic_plugin { heuristic_name = "break_ctor"; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }
