module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
open Ligo_prim
open Ast_typed
open Aggregation
open Main_errors
module Var = Simple_utils.Var

(* contract_info here is the optional information on the contract we are compiling
   It would be better if Self_ast_aggregated.all_contract was executing on
   all contracts and sub contracts *)
let compile_expression_in_context
    ~raise
    ~options
    ?(self_pass = true)
    ?(self_program = true)
    ?(force_uncurry = false)
    :  Ast_typed.contract_sig option -> Ast_typed.program -> Ast_typed.expression
    -> Ast_aggregated.expression
  =
 fun contract_info ctxt_typed exp ->
  let ctxt, exp =
    Trace.trace ~raise aggregation_tracer @@ Aggregation.compile_program exp ctxt_typed
  in
  let ctxt, exp =
    if self_pass
    then
      Trace.trace ~raise self_ast_aggregated_tracer
      @@ Self_ast_aggregated.all_program ~options ~self_program (ctxt, exp)
    else ctxt, exp
  in
  let exp = Ast_aggregated.context_apply ctxt exp in
  let exp =
    if self_pass
    then
      Trace.trace ~raise self_ast_aggregated_tracer
      @@ Self_ast_aggregated.all_aggregated_expression exp
    else exp
  in
  let exp =
    Option.value_map contract_info ~default:exp ~f:(fun { storage; parameter } ->
        Trace.trace ~raise self_ast_aggregated_tracer
        @@ Self_ast_aggregated.all_contract ~options parameter storage exp)
  in
  let exp = if force_uncurry then Ast_aggregated.Combinators.uncurry_wrap exp else exp in
  if self_pass then Self_ast_aggregated.remove_check_self exp else exp


let compile_expression ~raise ~options : Ast_typed.expression -> Ast_aggregated.expression
  =
 fun e ->
  let x = Trace.trace ~raise aggregation_tracer @@ compile_expression e in
  Trace.trace ~raise self_ast_aggregated_tracer
  @@ Self_ast_aggregated.all_expression ~options x


let apply_to_entrypoint_with_contract_type ~raise ~options
    :  Ast_typed.program -> Module_var.t list -> Ast_typed.contract_sig
    -> Ast_aggregated.expression
  =
 fun prg module_path contract_type ->
  let loc = Location.dummy in
  let { parameter = p_ty; storage = s_ty } = contract_type in
  let ty =
    t_arrow
      ~loc
      (t_pair ~loc p_ty s_ty)
      (t_pair ~loc (t_list ~loc (t_operation ~loc ())) s_ty)
      ()
  in
  let ep_expr =
    let open Ast_typed in
    match module_path with
    | [] -> e_a_variable ~loc Ligo_prim.Magic_vars.generated_main ty
    | _ ->
      e_module_accessor
        ~loc
        { module_path; element = Ligo_prim.Magic_vars.generated_main }
        ty
  in
  compile_expression_in_context ~raise ~options (Some contract_type) prg ep_expr


let apply_to_var ~raise ~options
    : Ast_typed.program -> string -> Ast_aggregated.expression
  =
 fun prg entrypoint ->
  let v = Value_var.of_input_var ~loc:Location.dummy entrypoint in
  let ty, _ =
    let sig_ =
      (* can't use the contract signature directly because
       it would force users to export declaration in Jsligo *)
      to_signature prg.pr_module
    in
    Trace.trace_option ~raise main_declaration_not_found
    @@ Ast_typed.get_sig_value [] v sig_
  in
  let var_ep = Ast_typed.(e_a_variable ~loc:Location.dummy v ty) in
  compile_expression_in_context ~raise ~options None prg var_ep


let assert_equal_contract_type ~raise
    :  Simple_utils.Runned_result.check_type -> Ast_typed.contract_sig
    -> Ast_typed.expression -> unit
  =
 fun c Ast_typed.{ parameter; storage } exp ->
  let ty =
    match c with
    | Check_storage -> storage
    | Check_parameter -> parameter
  in
  Trace.trace ~raise checking_tracer
  @@ Checking.assert_type_expression_eq Location.dummy (exp.type_expression, ty)


let apply_to_entrypoint_view ~raise ~options
    :  Module_var.t list -> Ast_typed.program
    -> (Ast_typed.type_expression * _ Binder.t) list -> Ast_aggregated.expression
  =
 fun module_path prg views_info ->
  let loc = Location.dummy in
  let aux : int -> _ -> Label.t * expression =
   fun i (view_ty, view_binder) ->
    let a_ty, s_ty, r_ty =
      (* at this point the self-pass on views has been applied, we assume the types are correct *)
      Trace.trace_option ~raise main_unknown @@ Ast_typed.get_view_form view_ty
    in
    let ty = t_arrow ~loc (t_pair ~loc a_ty s_ty) r_ty () in
    let ep_expr =
      let open Ast_typed in
      match module_path with
      | [] -> e_a_variable ~loc (Binder.get_var view_binder) ty
      | _ ->
        e_module_accessor ~loc { module_path; element = Binder.get_var view_binder } ty
    in
    Label.of_int i, ep_expr
  in
  let tuple_view =
    Ast_typed.ez_e_a_record ~loc ~layout:Layout.comb (List.mapi ~f:aux views_info)
  in
  let e = compile_expression_in_context ~raise ~options None prg tuple_view in
  Self_ast_aggregated.remove_check_self e


(*
  if only_ep, we only list the declarations with types fiting an entrypoint
  TODO (when we have module signature): extract declaration names from sig type
  Notes: a Ast_typed.program, would have to hold a signature too..
*)
let rec list_declarations
    ~(skip_generated : bool)
    (only_ep : bool)
    (m : Ast_typed.program)
    : Value_var.t list
  =
  let is_generated_main b =
    let v = Binder.get_var b in
    (not (Value_var.is_generated v))
    && String.is_prefix ~prefix:"$" (Value_var.to_name_exn v)
  in
  let should_skip b = skip_generated && is_generated_main b in
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match el.wrap_content with
      | D_irrefutable_match { pattern = { wrap_content = P_var binder; _ }; attr; _ }
      | D_value { binder; attr; _ }
        when attr.entry && (not (should_skip binder)) && not only_ep ->
        Binder.get_var binder :: prev
      | D_irrefutable_match { pattern = { wrap_content = P_var binder; _ }; attr; expr }
      | D_value { binder; attr; expr }
        when not attr.hidden ->
        if only_ep
        then
          if is_some (Ast_typed.Misc.get_type_of_contract expr.type_expression)
             && (not (should_skip binder))
             && is_generated_main binder
          then Binder.get_var binder :: prev
          else if is_some (Ast_typed.Misc.get_type_of_entrypoint expr.type_expression)
                  && (not (should_skip binder))
                  && attr.entry
          then Binder.get_var binder :: prev
          else prev
        else if not (should_skip binder)
        then Binder.get_var binder :: prev
        else prev
      | D_module_include _ -> prev (* What TODO here ? use signature *)
      | D_module
          { module_binder
          ; module_ = { module_content = M_struct m; signature; _ }
          ; module_attr
          ; _
          }
        when not module_attr.hidden ->
        (Ast_typed.{ pr_module = m; pr_sig = signature }
        |> list_declarations ~skip_generated only_ep
        |> List.map ~f:(fun v ->
               Value_var.of_input_var
                 ~loc:Location.generated
                 (Format.asprintf "%a." Module_var.pp module_binder
                 ^ Format.asprintf "%a" Value_var.pp v)))
        @ prev
      | D_value _
      | D_irrefutable_match _
      | D_type _
      | D_module _
      | D_signature _
      | D_import _ -> prev)
    ~init:[]
    m.pr_module


let list_type_declarations (m : Ast_typed.program) : Type_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match el.wrap_content with
      | D_type { type_binder; type_attr; _ } when type_attr.public -> type_binder :: prev
      | _ -> prev)
    ~init:[]
    m.pr_module


let get_modules_with_entries (prg : Ast_typed.program)
    : (Module_var.t list * Ast_typed.contract_sig) list
  =
  let module ModPathOrd = struct
    module T = struct
      type t =
        Module_var.t list * (Ast_typed.contract_sig[@compare.ignore] [@sexp.opaque])
      [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end
  in
  let contract_sig =
    match prg.pr_sig.sig_sort with
    | Ss_module ->
      let unit_type = Ast_typed.t_unit ~loc:Location.generated () in
      { parameter = unit_type; storage = unit_type }
    | Ss_contract contract_sig -> contract_sig
  in
  let rec aux ?(current_module_and_sig = [], contract_sig) (prg : Ast_typed.program)
      : ModPathOrd.Set.t
    =
    List.fold_left prg.pr_module ~init:ModPathOrd.Set.empty ~f:(fun acc decl ->
        match decl.wrap_content with
        | D_value { attr; _ } | D_irrefutable_match { attr; _ } ->
          if attr.entry then Set.add acc current_module_and_sig else acc
        | D_module
            { module_binder
            ; module_ = { module_content = M_struct pr_module; signature = pr_sig; _ }
            ; _
            } ->
          let current_module_and_sig =
            let current_module = module_binder :: fst current_module_and_sig in
            let contract_sig =
              match pr_sig.sig_sort with
              | Ss_module -> snd current_module_and_sig
              | Ss_contract contract_sig -> contract_sig
            in
            current_module, contract_sig
          in
          Ast_typed.{ pr_module; pr_sig } |> aux ~current_module_and_sig |> Set.union acc
        | D_module _ | D_type _ | D_module_include _ | D_signature _ | D_import _ -> acc)
  in
  aux prg
  |> Set.to_list
  |> List.fold_left ~f:(fun acc elt -> Tuple2.map_fst ~f:List.rev elt :: acc) ~init:[]
