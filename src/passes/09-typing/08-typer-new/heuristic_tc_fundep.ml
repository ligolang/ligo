(* selector / propagation rule for restricting a type class
     (α₁, α₂, …) ∈ { (τ₁₁, τ₁₂, …) , … }
   to the possible cases, given a second hypothesis of the form
     αᵢ = κ(β₁, β₂, …)
   It restricts the number of possible cases and replaces αᵢ in
   tuple of constrained variables so that the βⱼ are constrained
   instead.

   This rule can deduce a new assignment for other variables
   constrained by the typeclass if every possible type for that
   variable uses the same type constructor. *)

(* TODO: have a heuristic that restricts typeclass constraints with
   repeated or aliased type variables in the arguments, i.e. of the
   form […;x;…;y;…] ∈ […] where x and y are identical or aliased. *)

module Core = Typesystem.Core
open Ast_typed.Types
open Typesystem.Solver_types
open Trace
open Typer_common.Errors
module Map = RedBlackTrees.PolyMap
module BiMap = RedBlackTrees.PolyBiMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2

open Heuristic_tc_fundep_utils

(* ***********************************************************************
 * Selector
 * *********************************************************************** *)

let get_or_add_refined_typeclass : structured_dbs -> private_storage -> c_typeclass_simpl -> (private_storage * refined_typeclass) =
  fun dbs () tcs ->
  let open Heuristic_tc_fundep_utils in
  let tc = tc_to_constraint_identifier tcs in
  match Map.find_opt tc dbs.refined_typeclasses with
    Some x -> (), x
  | None -> failwith "internal error: couldn't find refined version of the typeclass constraint"

let is_variable_constrained_by_typeclass : structured_dbs -> type_variable -> refined_typeclass -> bool =
  fun dbs var refined_typeclass ->
  (* This won't work because the Set.mem function doesn't take into account the unification of two variables *)
  (* Set.mem var refined_typeclass *)
  List.exists
    (fun a ->
       Var.equal
         (UF.repr a dbs.aliases)
         (UF.repr var dbs.aliases))
  @@ Set.elements refined_typeclass.vars

(* Find typeclass constraints in the dbs which constrain c.tv
   This is useful for the typeclass constraints which do not exist yet in the private_storage. *)
let selector_by_ctor_in_db : private_storage -> structured_dbs -> c_constructor_simpl -> (private_storage * output_tc_fundep selector_outputs) =
  fun private_storage dbs c ->
  let typeclasses = (Constraint_databases.get_constraints_related_to c.tv dbs).tc in  
  let typeclasses = List.fold_map (get_or_add_refined_typeclass dbs) private_storage typeclasses in
  let typeclasses = List.filter (is_variable_constrained_by_typeclass dbs c.tv) typeclasses in
  let cs_pairs_db = List.map (fun tc -> { tc ; c }) typeclasses in
  private_storage, cs_pairs_db

(* Find typeclass constraints in the private_storage which constrain c.tv *)
let selector_by_ctor_in_private_storage : private_storage -> structured_dbs -> c_constructor_simpl -> (private_storage * output_tc_fundep selector_outputs) =
  fun private_storage dbs c ->
  let refined_typeclasses = List.map snd @@ Map.bindings dbs.refined_typeclasses in
  let typeclasses =
    List.filter
      (is_variable_constrained_by_typeclass dbs c.tv)
      refined_typeclasses in
  let cs_pairs_db = List.map (fun tc -> { tc ; c }) typeclasses in
  private_storage, cs_pairs_db

let selector_by_ctor : private_storage -> structured_dbs -> c_constructor_simpl -> (private_storage * output_tc_fundep selector_outputs) =
  fun private_storage dbs c ->
  let private_storage, cs_pairs_in_db = selector_by_ctor_in_db private_storage dbs c in
  let private_storage, cs_pairs_in_private_storage = selector_by_ctor_in_private_storage private_storage dbs c in
  private_storage, cs_pairs_in_db @ cs_pairs_in_private_storage

(* Find constructor constraints α = κ(β …) where α is one of the
   variables constrained by the (refined version of the) typeclass
   constraint tcs. *)
let selector_by_tc : private_storage -> structured_dbs -> c_typeclass_simpl -> (private_storage * output_tc_fundep selector_outputs) =
  fun private_storage dbs tcs ->
  let private_storage, tc = get_or_add_refined_typeclass dbs private_storage tcs in
  (* TODO: this won't detect already-existing constructor
     constraints that would apply to future versions of the refined
     typeclass. *)
  let aux tv =
    (* Find the constructor constraints which apply to tv. *)
    (* Since we are only refining the typeclass one type expression
       node at a time, we only need the top-level assignment for
       that variable, e.g. α = κ(βᵢ, …). We can therefore look
       directly in the assignments. *)
    match PolyMap.find_opt tv dbs.assignments with
      Some c -> [({ tc ; c } : output_tc_fundep)]
    | None   -> [] in
  private_storage, (List.flatten @@ List.map aux tc.refined.args)

let selector : (type_constraint_simpl , output_tc_fundep , private_storage) selector =
  fun type_constraint_simpl private_storage dbs ->
  match type_constraint_simpl with
    SC_Constructor c  -> selector_by_ctor private_storage dbs c
  | SC_Row r          -> ignore r; failwith "TODO: call selector_by_ctor private_storage dbs r"
  | SC_Alias       _  -> private_storage, [] (* TODO: ? *)
  | SC_Poly        _  -> private_storage, [] (* TODO: ? *)
  | SC_Typeclass   tc -> selector_by_tc private_storage dbs tc

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)

let restrict_one (c : c_constructor_simpl) (allowed : type_value) =
  match c, allowed.t with
  | { reason_constr_simpl=_; tv=_; c_tag; tv_list }, P_constant { p_ctor_tag; p_ctor_args } ->
    if Ast_typed.Compare.constant_tag c_tag p_ctor_tag = 0
    then if List.compare_lengths tv_list p_ctor_args = 0
      then Some p_ctor_args
      else None (* case removed because type constructors are different *)
    else None   (* case removed because argument lists are of different lengths *)
  | _, P_row _ -> failwith "TODO: support P_row similarly to P_constant"
  | _, (P_forall _ | P_variable _ | P_apply _) -> None (* TODO: does this mean that we can't satisfy these constraints? *)

(* Restricts a typeclass to the possible cases given v = k(a, …) in c *)
let restrict (({ reason_constr_simpl = _; tv = _; c_tag = _; tv_list } as c) : c_constructor_simpl) (tcs : c_typeclass_simpl) =
  (* TODO: this is bogus if there is shadowing *)
  let index = List.find_index (Var.equal c.tv) tcs.args in
  (* Eliminate the impossible cases and splice in the type arguments
     for the possible cases: *)
  let aux allowed_tuple =
    splice_or_none (fun allowed -> restrict_one c allowed) index allowed_tuple in
  let tc = List.filter_map aux tcs.tc in
  (* Replace the corresponding typeclass argument with the type
     variables passed to the type constructor *)
  let args = splice (fun _arg -> tv_list) index tcs.args in
  let id_typeclass_simpl = tcs.id_typeclass_simpl in
  { reason_typeclass_simpl = tcs.reason_typeclass_simpl; is_mandatory_constraint = tcs.is_mandatory_constraint; id_typeclass_simpl ; tc ; args }

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
  let%bind cleaned = transpose_back (tcs.reason_typeclass_simpl, tcs.is_mandatory_constraint) tcs.id_typeclass_simpl vars_and_possibilities in
  ok { deduced ; cleaned }

let propagator : (output_tc_fundep, private_storage , typer_error) propagator =
  fun private_storage _dbs selected ->
  (* The selector is expected to provide constraints with the shape (α
     = κ(β, …)) and to update the private storage to keep track of the
     refined typeclass *)
  let restricted = restrict selected.c selected.tc.refined in
  let%bind {deduced ; cleaned} = deduce_and_clean restricted in
  let cleaned : refined_typeclass = make_refined_typeclass cleaned ~original:selected.tc.original in
  (* TODO: this is because we cannot return a simplified constraint,
     and instead need to retun a constraint as it would appear if it
     came from the program (generated by the ill-named module
     "Wrap"). type_constraint_simpl is more or less a subset of
     type_constraint, but some parts have been shuffled
     around. Hopefully this can be sorted out so that we don't need a
     dummy value for the srcloc and maybe even so that we don't need a
     conversion (one may dream). *)
  let cleaned : type_constraint = {
    reason = cleaned.refined.reason_typeclass_simpl;
    c = C_typeclass {
        tc_args = (List.map (fun x -> { tsrc = "no idea"; t = P_variable x }) cleaned.refined.args);
        typeclass = cleaned.refined.tc;
      }
  } in
  let aux (x : c_constructor_simpl) : type_constraint = {
    reason = "inferred: only possible type for that variable in the typeclass";
    c = C_equation {
        aval = { tsrc = "?" ;
                 t    = P_variable x.tv };
        bval = { tsrc = "? generated" ;
                 t    = P_constant { p_ctor_tag  = x.c_tag ;
                                     p_ctor_args =
                                       List.map
                                         (fun v -> { tsrc = "? probably generated" ;
                                                     t    = P_variable v})
                                         x.tv_list ; } } } } in
  let deduced : type_constraint list = List.map aux deduced in
  ok (private_storage, [
      {
        remove_constraints = [SC_Typeclass selected.tc.refined];
        add_constraints = cleaned :: deduced;
        justification = "no removal so no justification needed"
      }
    ])

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)

let heuristic =
  Propagator_heuristic
    {
      selector ;
      propagator ;
      printer = Ast_typed.PP.output_tc_fundep ;
      comparator = Solver_should_be_generated.compare_output_tc_fundep ;
      initial_private_storage = () ;
    }

