module I = Ast_unified
module O = Ast_core
open Passes.Pass_type
module Errors = Passes.Errors
module Selector = Passes.Pass_type.Selector
open Simple_utils.Function
module ModRes = Preprocessor.ModRes

type flags =
  { initial_node_check : bool
  ; duplicate_identifier : bool
  ; for_to_while_loop : bool
  ; restrict_projection : bool
  ; export : bool
  ; freeze_operators : Syntax_types.t
  ; list_as_function : bool
  ; array_to_tuple : bool
  ; match_as_function : bool
  ; object_to_record : bool
  ; detect_recursion : bool
  ; hack_literalize_jsligo : bool
  ; named_fun : bool
  ; t_app_michelson_types : Syntax_types.t
  ; projections : Syntax_types.t
  ; mod_res : ModRes.t option
  ; wildcards : bool
  }

let passes ~(flags : flags) : (module T) list =
  let open Passes in
  let { initial_node_check
      ; duplicate_identifier
      ; for_to_while_loop
      ; restrict_projection
      ; export
      ; list_as_function
      ; array_to_tuple
      ; match_as_function
      ; object_to_record
      ; detect_recursion
      ; freeze_operators
      ; hack_literalize_jsligo
      ; named_fun
      ; t_app_michelson_types
      ; projections
      ; mod_res
      ; wildcards
      }
    =
    flags
  in
  let always = true in
  let entry
      : type a. (module T with type flag_arg = a) -> flag:bool -> arg:a -> (module T)
    =
   fun (module P) ~flag ~arg ->
    P.set_flag ~enable:flag arg;
    (module P)
  in
  [ entry (module Initial_node_check) ~flag:initial_node_check ~arg:()
  ; entry (module For_to_while_loop) ~flag:always ~arg:for_to_while_loop
  ; entry (module Escaped_variables) ~flag:always ~arg:()
  ; entry (module Wildcards) ~flag:wildcards ~arg:()
  ; entry (module Duplicate_identifier) ~flag:duplicate_identifier ~arg:()
  ; entry (module Linear_signature) ~flag:always ~arg:()
  ; entry (module Restrict_projections) ~flag:restrict_projection ~arg:()
  ; entry (module Break_continue_outside) ~flag:always ~arg:()
  ; entry (module Single_switch_block) ~flag:always ~arg:()
  ; entry (module Export_program_entry) ~flag:export ~arg:()
  ; entry (module Export_declaration) ~flag:export ~arg:()
  ; entry (module Top_level_restriction) ~flag:always ~arg:()
  ; entry (module Pattern_constructor_application) ~flag:always ~arg:()
  ; entry (module Pattern_restriction) ~flag:always ~arg:()
  ; entry (module Pattern_heuristic) ~flag:always ~arg:()
  ; entry (module Unpuning) ~flag:always ~arg:()
  ; entry (module Module_open_restriction) ~flag:always ~arg:()
  ; entry (module Import_restriction) ~flag:always ~arg:()
  ; entry (module External_hack) ~flag:always ~arg:()
  ; entry (module Linearity) ~flag:always ~arg:()
  ; entry (module T_constant) ~flag:always ~arg:()
  ; entry (module T_arg) ~flag:always ~arg:()
  ; entry (module Constructor_application) ~flag:always ~arg:()
  ; entry (module Standalone_constructor_removal) ~flag:always ~arg:()
  ; entry (module Type_abstraction_declaration) ~flag:always ~arg:()
  ; entry (module Sum_type_helper_generator) ~flag:always ~arg:()
  ; entry (module Named_fun) ~flag:named_fun ~arg:()
  ; entry (module Reverse_application) ~flag:always ~arg:()
  ; entry (module Prefix_postfix_operators) ~flag:always ~arg:()
  ; entry (module Freeze_operators) ~flag:always ~arg:freeze_operators
  ; entry (module Literalize_annotated) ~flag:always ~arg:()
  ; entry (module Of_file) ~flag:always ~arg:mod_res
  ; entry (module List_as_function) ~flag:list_as_function ~arg:()
  ; entry (module Array_to_tuple) ~flag:array_to_tuple ~arg:()
  ; entry (module Match_tc39) ~flag:match_as_function ~arg:()
  ; entry (module Object_to_record) ~flag:object_to_record ~arg:()
  ; entry (module Hack_literalize_jsligo) ~flag:hack_literalize_jsligo ~arg:()
  ; entry (module Restrict_t_app) ~flag:always ~arg:()
  ; entry (module T_app_michelson_types) ~flag:always ~arg:t_app_michelson_types
  ; entry (module Multi_bindings) ~flag:always ~arg:()
  ; entry (module Loop_variable) ~flag:always ~arg:()
  ; entry (module Disc_union_types) ~flag:always ~arg:()
  ; entry (module Returns) ~flag:always ~arg:()
  ; entry (module Reduce_switch) ~flag:always ~arg:()
  ; entry (module Structural_updates) ~flag:always ~arg:()
  ; entry (module Map_lookup) ~flag:always ~arg:()
  ; entry (module Freeze_containers) ~flag:always ~arg:()
  ; entry (module Unstate) ~flag:always ~arg:()
  ; entry (module Projections) ~flag:always ~arg:projections
  ; entry (module Assign_transitivity) ~flag:always ~arg:()
  ; entry (module Reduce_sequence) ~flag:always ~arg:()
  ; entry (module Let_syntax) ~flag:always ~arg:()
  ; entry (module Generalize_functions) ~flag:always ~arg:()
  ; entry (module Detect_recursive) ~flag:detect_recursion ~arg:()
  ; entry (module Curry) ~flag:always ~arg:()
  ; entry (module Tuple_as_record) ~flag:always ~arg:()
  ; entry (module If_as_pattern_match) ~flag:always ~arg:()
  ; entry (module Restrict_typed_pattern) ~flag:always ~arg:()
  ; entry (module Compute_layout.Normalize_layout) ~flag:always ~arg:()
  ; entry (module Compute_layout.Normalize_no_layout) ~flag:always ~arg:()
  ]


let extract_flags_from_options : disable_initial_check:bool -> Compiler_options.t -> flags
  =
 fun ~disable_initial_check options ->
  let syntax =
    Option.value_map options.frontend.syntax ~default:Syntax_types.CameLIGO ~f:Fun.id
  in
  let is_jsligo = Syntax_types.equal syntax JsLIGO in
  let is_cameligo = Syntax_types.equal syntax CameLIGO in
  let duplicate_identifier = if options.frontend.transpiled then false else is_jsligo in
  let mod_res =
    Option.bind ~f:Preprocessor.ModRes.make options.Compiler_options.frontend.project_root
  in
  { initial_node_check = not disable_initial_check
  ; duplicate_identifier
  ; for_to_while_loop = options.frontend.warn_infinite_loop
  ; restrict_projection = is_jsligo
  ; export = is_jsligo
  ; list_as_function = is_jsligo
  ; array_to_tuple = is_jsligo
  ; match_as_function = is_jsligo
  ; object_to_record = is_jsligo
  ; detect_recursion = is_jsligo
  ; freeze_operators = syntax
  ; hack_literalize_jsligo = is_jsligo
  ; named_fun = is_jsligo
  ; t_app_michelson_types = syntax
  ; projections = syntax
  ; mod_res
  ; wildcards = is_cameligo
  }


let get_passes ~options ~disable_initial_check =
  let flags = extract_flags_from_options ~disable_initial_check options in
  passes ~flags


let get_passes_no_options syntax =
  let flags =
    extract_flags_from_options
      ~disable_initial_check:false
      Compiler_options.(make ~syntax ~raw_options:(Raw_options.make ()) ())
  in
  passes ~flags


let execute_nanopasses
    ~raise
    ~options
    ~sort
    ?stop_before
    ?(disable_initial_check = false)
    prg
  =
  compile_passes
    ~raise
    ?stop_before
    ~sort
    (get_passes ~options ~disable_initial_check)
    prg


let decompile_program ~raise ~syntax : O.program -> I.program =
  decompile_passes ~raise ~sort:Selector.program (get_passes_no_options syntax)
  <@ Trivial.From_core.program ~raise


let decompile_expression ~raise ~syntax : O.expression -> I.expr =
  decompile_passes ~raise ~sort:Selector.expr (get_passes_no_options syntax)
  <@ Trivial.From_core.expression ~raise


let decompile_pattern ~raise ~syntax : O.type_expression option O.Pattern.t -> I.pattern =
  decompile_passes ~raise ~sort:Selector.pattern (get_passes_no_options syntax)
  <@ Trivial.From_core.pattern ~raise


let decompile_ty_expr ~raise ~syntax =
  decompile_passes ~raise ~sort:Selector.ty_expr (get_passes_no_options syntax)
  <@ Trivial.From_core.type_expression ~raise


let decompile_sig_expr ~raise ~syntax =
  decompile_passes ~raise ~sort:Selector.sig_expr (get_passes_no_options syntax)
  <@ Trivial.From_core.signature ~raise
  <@ fun (O.{ Location.wrap_content = content; _ } as sig_expr) ->
  let new_content =
    match content with
    | O.S_sig { items } ->
      O.S_sig
        { items =
            List.filter items ~f:(function
                | S_module _ | S_module_type _ -> false
                | S_include _ | S_value _ | S_type _ | S_type_var _ -> true)
        }
    | O.S_path _ -> content
  in
  { sig_expr with wrap_content = new_content }


let compile_program ~raise ~(options : Compiler_options.t) ?stop_before
    : I.program -> O.program
  =
  Trivial.To_core.program ~raise
  <@ compile_passes
       ~raise
       ?stop_before
       ~sort:Selector.program
       (get_passes ~options ~disable_initial_check:false)


let compile_expression
    ~raise
    ~(options : Compiler_options.t)
    ?(disable_initial_check = false)
    : I.expr -> O.expression
  =
  Trivial.To_core.expression ~raise
  <@ compile_passes
       ~raise
       ~sort:Selector.expr
       (get_passes ~options ~disable_initial_check)
