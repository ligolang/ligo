(* selector / propagation rule for restricting a type class
     (α₁, α₂, …) ∈ { (τ₁₁, τ₁₂, …) , … }
   to the possible cases, given a second hypothesis of the form
     αᵢ = κ(β₁, β₂, …)
      or
     αᵢ = ρ(ℓᵢ : βᵢ, …)
   It restricts the number of possible cases and replaces αᵢ in
   tuple of constrained variables so that the βⱼ are constrained
   instead.

   This rule can deduce a new assignment for other variables
   constrained by the typeclass if every possible type for that
   variable uses the same type constructor. *)

(* TODO: have a heuristic that restricts typeclass constraints with
   repeated or aliased type variables in the arguments, i.e. of the
   form […;x;…;y;…] ∈ […] where x and y are identical or aliased. *)

open Ast_typed.Types
open Typesystem.Solver_types
open Trace
open Typer_common.Errors
open Ast_typed.Reasons
open Database_plugins.All_plugins

open Heuristic_tc_fundep_utils

type selector_output = output_tc_fundep

(* ***********************************************************************
 * Selector
 * *********************************************************************** *)

(* Find typeclass constraints in the dbs which constrain c.tv *)
let selector_by_ctor : _ indexes -> c_constructor_simpl -> (output_tc_fundep selector_outputs) =
  fun indexes c ->
  let typeclasses = (TypeclassesConstraining.get_refined_typeclasses_constraining_list c.tv indexes) in
  let cs_pairs_db = List.map (fun tc -> { tc ; c = `Constructor c }) typeclasses in
  cs_pairs_db

let selector_by_row : _ indexes -> c_row_simpl -> (output_tc_fundep selector_outputs) =
  fun indexes r ->
  let typeclasses = (TypeclassesConstraining.get_refined_typeclasses_constraining_list r.tv indexes) in
  let cs_pairs_db = List.map (fun tc -> { tc ; c = `Row r }) typeclasses in
  cs_pairs_db

(* Find constructor constraints α = κ(β …) where α is one of the
   variables constrained by the (refined version of the) typeclass
   constraint tcs. *)
let selector_by_tc : _ indexes -> c_typeclass_simpl -> (output_tc_fundep selector_outputs) =
  fun indexes tcs ->
  let tc = RefinedTypeclasses.find tcs indexes#refined_typeclasses in
  (* TODO: this won't detect already-existing constructor
     constraints that would apply to future versions of the refined
     typeclass. *)
  let aux tv =
    (* Find the constructor constraints which apply to tv. *)
    (* Since we are only refining the typeclass one type expression
       node at a time, we only need the top-level assignment for
       that variable, e.g. α = κ(βᵢ, …). We can therefore look
       directly in the assignments. *)
    match Assignments.find_opt tv indexes#assignments with
    | Some cr -> [({ tc ; c = cr } : output_tc_fundep)]
    | None   -> [] in
  List.flatten @@ List.map aux tc.refined.args

let selector : type_constraint_simpl -> _ indexes -> selector_output list =
  fun type_constraint_simpl indexes ->
  match type_constraint_simpl with
    SC_Constructor c  -> selector_by_ctor indexes c
  | SC_Row r          -> selector_by_row indexes r
  | SC_Alias       _  -> [] (* TODO: this case should go away since aliases are handled by the solver structure *)
  | SC_Poly        _  -> []
  | SC_Typeclass   tc -> selector_by_tc indexes tc

(* When (αᵢ, …) ∈ { (τ, …) , … } and β = κ(δ …) are in the db,
   aliasing α and β should check if they are non-empty, and in that
   case produce a selector_output for all pairs. This will involve a
   lookup to see if α is constrained by a typeclass
   (typeclasses_constraining indexer). Add to this the logic for
   refined_typeclass vs. typeclass. *)

   (*
    1 , 2 , 3 , 4
    -> (1,3) , (1,4)
  
    +5 -> 1 , 2 , 3 , 4 , 5

   *)

let alias_selector : type_variable -> type_variable -> _ indexes -> selector_output list =
  fun a b indexes ->
  let a_tcs = (TypeclassesConstraining.get_refined_typeclasses_constraining_list a indexes) in
  let b_tcs = (TypeclassesConstraining.get_refined_typeclasses_constraining_list b indexes) in
  let a_lhs_constr = GroupedByVariable.get_constraints_by_lhs a indexes#grouped_by_variable in
  let b_lhs_constr = GroupedByVariable.get_constraints_by_lhs b indexes#grouped_by_variable in
  let a_ctors = List.map (fun a -> `Constructor a) a_lhs_constr.constructor in
  let a_rows = List.map (fun a -> `Row a) a_lhs_constr.row in
  let b_ctors = List.map (fun a -> `Constructor a) b_lhs_constr.constructor in
  let b_rows = List.map (fun a -> `Row a) b_lhs_constr.row in
  List.flatten @@
  List.map
    (fun tc ->
       List.map
         (fun c ->
            { tc ; c })
         (a_ctors @ b_ctors @ a_rows @ b_rows ))
    (a_tcs @ b_tcs)

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)

let restrict_one (cr : constructor_or_row) (allowed : type_value) =
  match cr, allowed.wrap_content with
  | `Constructor { reason_constr_simpl=_; tv=_; c_tag; tv_list }, P_constant { p_ctor_tag; p_ctor_args } ->
    if Ast_typed.Compare.constant_tag c_tag p_ctor_tag = 0
    then if List.compare_lengths tv_list p_ctor_args = 0
      then Some p_ctor_args
      else None (* case removed because type constructors are different *)
    else None   (* case removed because argument lists are of different lengths *)
  | `Row _, P_row _ -> failwith "TODO: support P_row similarly to P_constant"
  | _, (P_forall _ | P_variable _ | P_apply _ | P_row _ | P_constant _) -> None (* TODO: does this mean that we can't satisfy these constraints? *)

(* Restricts a typeclass to the possible cases given v = k(a, …) in c *)
let restrict (constructor_or_row : constructor_or_row) (tcs : c_typeclass_simpl) =
  let (tv_list, tv) = match constructor_or_row with
    | `Row r -> LMap.to_list r.tv_map , r.tv
    | `Constructor c -> c.tv_list , c.tv
  in
  (* TODO: this is bogus if there is shadowing *)
  let index = List.find_index (Var.equal tv) tcs.args in
  (* Eliminate the impossible cases and splice in the type arguments
     for the possible cases: *)
  let aux allowed_tuple =
    splice_or_none (fun allowed -> restrict_one constructor_or_row allowed) index allowed_tuple in
  let tc = List.filter_map aux tcs.tc in
  (* Replace the corresponding typeclass argument with the type
     variables passed to the type constructor *)
  let args = splice (fun _arg -> tv_list) index tcs.args in
  let id_typeclass_simpl = tcs.id_typeclass_simpl in
  { reason_typeclass_simpl = tcs.reason_typeclass_simpl; original_id = tcs.original_id; is_mandatory_constraint = tcs.is_mandatory_constraint; id_typeclass_simpl ; tc ; args }

(* input:
     x ? [ map3( nat , unit , float ) ; map3( bytes , mutez , float ) ]
   output:
     true,
     [ x = map( m , n , o ) ; o = float ( ) ],
     [ m ? [ nat  ; bytes ]
       n ? [ unit ; mutez ] ] *)
let replace_var_and_possibilities_1 ((x : type_variable) , (possibilities_for_x : type_value list)) =
  let%bind tags_and_args = bind_map_list get_tag_and_args_of_constant possibilities_for_x in
  let tags_of_constructors, arguments_of_constructors = List.split @@ tags_and_args in
  match all_equal Ast_typed.Compare.constant_tag tags_of_constructors with
  | Different ->
    (* The "changed" boolean return indicates whether any update was done.
       It is used to detect when the variable doesn't need any further cleanup. *)
    ok ( false, [ (x, possibilities_for_x) ], [] )            (* Leave as-is, don't deduce anything *)
  | Empty ->
    (* TODO: keep track of the constraints used to refine the
       typeclass so far. *)
    (* fail @@ typeclass_error
     *   "original expected by typeclass"
     *   "actual partially guessed so far (needs a recursive substitution)" *)
    failwith "type error: the typeclass does not allow any type for \
              the variable %a:PP_variable:x at this point"
  | All_equal_to c_tag ->
    match arguments_of_constructors with
    | [] -> failwith "the typeclass does not allow any possibilities \
                      for the variable %a:PP_variable:x at this point"
    | (arguments_of_first_constructor :: _) as arguments_of_constructors ->
      let fresh_vars = List.map (fun _arg -> Var.fresh_like x) arguments_of_first_constructor in
      let deduced : c_constructor_simpl = {
        reason_constr_simpl = "inferred because it is the only remaining possibility at this point according to the typeclass [TODO:link to the typeclass here]" ;
        is_mandatory_constraint = false;
        tv = x;
        c_tag ;
        tv_list = fresh_vars
      } in
      (* discard the identical tags, splice their arguments instead, and deduce the x = tag(…) constraint *)
      let sub_part_of_typeclass = {
        reason_typeclass_simpl = Format.asprintf
            "sub-part of a typeclass: expansion of the possible \
             arguments for the constructor associated with %a"
            Ast_typed.PP.type_variable x;
        is_mandatory_constraint = false;
        original_id = None;     (* TODO this and the is_mandatory_constraint are not actually used, should use a different type without these fields. *)
        id_typeclass_simpl = ConstraintIdentifier (-1L) ; (* TODO: this and the reason_typeclass_simpl should simply not be used here *)
        args = fresh_vars ;
        tc = arguments_of_constructors ;
      } in
      let%bind possibilities_alist = transpose sub_part_of_typeclass in
      (* The "changed" boolean return indicates whether any update was done.
         It is used to detect when the variable doesn't need any further cleanup. *)
      ok (true, possibilities_alist, [deduced])

let rec replace_var_and_possibilities_rec ((x : type_variable) , (possibilities_for_x : type_value list)) =
  let open Rope.SimpleRope in
  let%bind (changed1, possibilities_alist, deduced) = replace_var_and_possibilities_1 (x, possibilities_for_x) in
  if changed1 then
    (* the initial var_and_possibilities has been changed, recursively
       replace in the resulting vars and their possibilities, and
       aggregate the deduced constraints. *)
    let%bind (_changed, vp, more_deduced) = replace_vars_and_possibilities_list possibilities_alist in
    ok (true, vp, pair (rope_of_list deduced) more_deduced)
  else
    ok (changed1, rope_of_list possibilities_alist, rope_of_list deduced)

and replace_vars_and_possibilities_list possibilities_alist =
  let open Rope.SimpleRope in
  bind_fold_list
    (fun (changed_so_far, vps, ds) x ->
       let%bind (changed, vp, d) = replace_var_and_possibilities_rec x in
       ok (changed_so_far || changed, pair vps vp, pair ds d))
    (false, empty, empty)
    possibilities_alist

let replace_vars_and_possibilities possibilities_alist =
  let open Rope.SimpleRope in
  let%bind (_changed, possibilities_alist, deduced) = replace_vars_and_possibilities_list possibilities_alist in
  ok (list_of_rope possibilities_alist, list_of_rope deduced)

let deduce_and_clean : c_typeclass_simpl -> (deduce_and_clean_result, _) result = fun tcs ->
  (* ex.   [ x                             ; z      ]
       ∈ [ [ map3( nat   , unit  , float ) ; int    ] ;
           [ map3( bytes , mutez , float ) ; string ] ] *)
  let%bind possibilities_alist = transpose tcs in
  (* ex. [ x ? [ map3( nat , unit , float ) ; map3( bytes , mutez , float ) ; ] ;
           z ? [ int                        ; string                        ; ] ; ] *)
  let%bind (vars_and_possibilities, deduced) = replace_vars_and_possibilities possibilities_alist in
  (* ex. possibilities_alist:
         [   fresh_x_1 ? [ nat   ; bytes  ] ;
             fresh_x_2 ? [ unit  ; mutez  ] ;
             y         ? [ int   ; string ]     ]
         deduced:
         [ x         = map3  ( fresh_x_1 , fresh_x_2 , fresh_x_3 ) ;
           fresh_x_3 = float (                                   ) ; ] *)
  let%bind cleaned = transpose_back (tcs.reason_typeclass_simpl, tcs.original_id, tcs.is_mandatory_constraint) tcs.id_typeclass_simpl vars_and_possibilities in
  ok { deduced ; cleaned }

let propagator : (output_tc_fundep, typer_error) propagator =
  fun selected ->
  (* The selector is expected to provide constraints with the shape (α
     = κ(β, …)) and to update the private storage to keep track of the
     refined typeclass *)
  let restricted = restrict selected.c selected.tc.refined in
  let%bind {deduced ; cleaned} = deduce_and_clean restricted in
  let cleaned : refined_typeclass = make_refined_typeclass cleaned ~original:selected.tc.original in
  (* TODO: this is because we cannot return a simplified constraint,
     and instead need to retun a constraint as it would appear if it
     came from the module (generated by the ill-named module
     "Wrap"). type_constraint_simpl is more or less a subset of
     type_constraint, but some parts have been shuffled
     around. Hopefully this can be sorted out so that we don't need a
     dummy value for the srcloc and maybe even so that we don't need a
     conversion (one may dream). *)
  let tc_args = List.map (fun x -> wrap (Todo "no idea") @@ P_variable x) cleaned.refined.args in
  let cleaned : type_constraint = {
      reason = cleaned.refined.reason_typeclass_simpl;
      c = C_typeclass {
        tc_args ;
        typeclass = cleaned.refined.tc;
        original_id = Some selected.tc.original;
      }
    }
  in
  let aux (x : c_constructor_simpl) : type_constraint = {
    reason = "inferred: only possible type for that variable in the typeclass";
    c = C_equation {
      aval = wrap (Todo "?") @@ P_variable x.tv ;
      bval = wrap (Todo "? generated") @@
              P_constant {
                p_ctor_tag  = x.c_tag ;
                p_ctor_args = List.map
                  (fun v -> wrap (Todo "? probably generated") @@ P_variable v)
                  x.tv_list ;
              }
      }
    }
  in
  let deduced : type_constraint list = List.map aux deduced in
  ok [
      {
        remove_constraints = [SC_Typeclass selected.tc.refined];
        add_constraints = cleaned :: deduced;
        proof_trace = Axiom (HandWaved "cut with the following (cleaned => removed_typeclass) to show that the removal does not lose info, (removed_typeclass => selected.c => cleaned) to show that the cleaned vesion does not introduce unwanted constraints.")
      }
    ]

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)

let printer = Ast_typed.PP.output_tc_fundep
let printer_json = Ast_typed.Yojson.output_tc_fundep
let comparator = Solver_should_be_generated.compare_output_tc_fundep

let heuristic = Heuristic_plugin { selector; alias_selector; propagator; printer; printer_json; comparator }
