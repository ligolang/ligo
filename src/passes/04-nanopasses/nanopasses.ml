module I = Ast_unified
module O = Ast_core
open Passes.Pass_type
module Errors = Passes.Errors
open Simple_utils.Function

(* Note:
  In passes, there should be a set of options for forward (compile)
  and a set of options for backward (decompile) *)
let passes
    ~(raise : (Passes.Errors.t, _) Simple_utils.Trace.raise)
    ~(syntax : Syntax_types.t)
    ~disable_initial_check
    ~duplicate_identifier
  =
  let open Passes in
  [ Initial_node_check.pass ~raise ~disable_initial_check
  ; Duplicate_identifier.pass ~raise ~enable:duplicate_identifier
  ; Restrict_projections.pass ~raise ~syntax
  ; Single_switch_block.pass
  ; Export_declaration.pass ~raise
  ; Top_level_restriction.pass ~raise
  ; Contract_hack.pass ~raise
  ; Pattern_restriction.pass ~raise
  ; Unpuning.pass ~raise
  ; Module_open_restriction.pass ~raise
  ; Import_restriction.pass ~raise
  ; External_hack.pass
  ; Linearity.pass ~raise
  ; T_constant.pass
  ; T_arg.pass
  ; Type_abstraction_declaration.pass ~raise
  ; Constructor_application.pass ~raise ~syntax
  ; Pattern_constructor_application.pass ~raise ~syntax
  ; Standalone_constructor_removal.pass ~raise
  ; Named_fun.pass ~raise
  ; E_rev_app.pass ~raise
  ; Freeze_operators.pass ~raise ~syntax
  ; Literalize_annotated.pass ~raise
  ; List_as_function.pass ~raise ~syntax
  ; Array_to_tuple.pass ~raise ~syntax
  ; Match_as_function.pass ~raise ~syntax
  ; Object_to_record.pass ~raise ~syntax
  ; Hack_literalize_jsligo.pass ~raise ~syntax
  ; Restrict_t_app.pass ~raise
  ; T_app_michelson_types.pass ~raise ~syntax
  ; Multi_bindings.pass ~raise
  ; Loop_variable.pass ~raise
  ; Disc_union_types.pass ~raise
  ; Returns.pass ~raise
  ; Reduce_switch.pass ~raise
  ; Structural_updates.pass ~raise
  ; Map_lookup.pass ~raise
  ; Freeze_containers.pass ~raise
  ; Projections.pass ~raise ~syntax
  ; Unstate.pass ~raise
  ; Assign_transitivity.pass ~raise
  ; Reduce_sequence.pass ~raise
  ; Let_syntax.pass ~raise
  ; Generalize_functions.pass ~raise
  ; Detect_recursive.pass ~raise ~syntax
  ; Curry.pass ~raise
  ; Tuple_as_record.pass ~raise
  ; If_as_pattern_match.pass ~raise
  ; Restrict_typed_pattern.pass ~raise
  ; Compute_layout.pass1
  ; Compute_layout.pass2 ~raise
  ]


let extract_options : Compiler_options.t -> Syntax_types.t * bool =
 fun options ->
  let syntax =
    Option.value_map options.frontend.syntax ~default:Syntax_types.CameLIGO ~f:Fun.id
  in
  let duplicate_identifier =
    if options.frontend.transpiled then false else Syntax_types.equal syntax JsLIGO
  in
  syntax, duplicate_identifier


let get_passes ~raise ~syntax ~disable_initial_check =
  passes ~raise ~syntax ~disable_initial_check


let nanopasses_program_until ~raise ~options ?stop_before prg =
  let passes =
    let syntax, duplicate_identifier = extract_options options in
    get_passes ~raise ~syntax ~duplicate_identifier ~disable_initial_check:false
  in
  nanopasses_until passes ?stop_before ~selector:program_selector prg


let decompile_program ~raise ~syntax : O.program -> I.program =
 fun _ ->
  ignore (raise, syntax);
  assert false


let decompile_expression ~raise ~syntax : O.expression -> I.expr =
  let passes =
    get_passes ~raise ~syntax ~disable_initial_check:false ~duplicate_identifier:true
  in
  decompile_with_passes (List.map ~f:expr_selector passes) <@ Trivial.From_core.expression


let decompile_pattern ~raise ~syntax : O.type_expression option O.Pattern.t -> I.pattern =
  let passes =
    get_passes ~raise ~syntax ~disable_initial_check:false ~duplicate_identifier:true
  in
  decompile_with_passes (List.map ~f:pattern_selector passes) <@ Trivial.From_core.pattern


let decompile_ty_expr ~raise ~syntax _ =
  ignore (raise, syntax);
  assert false


let compile_program ~raise ~(options : Compiler_options.t) : I.program -> O.program =
 fun prg -> Trivial.To_core.program ~raise (nanopasses_program_until ~raise ~options prg)


let compile_expression
    ~raise
    ~(options : Compiler_options.t)
    ?(disable_initial_check = false)
    : I.expr -> O.expression
  =
  let syntax, duplicate_identifier = extract_options options in
  let passes = get_passes ~raise ~syntax ~disable_initial_check ~duplicate_identifier in
  Trivial.To_core.expression ~raise
  <@ compile_with_passes (List.map ~f:expr_selector passes)
