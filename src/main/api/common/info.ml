open Api_helpers
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options
module Formatter = Ligo_formatter

let measure_contract (raw_options : Raw_options.t) source_file display_format no_colour ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    Formatter.contract_size_format
  @@ fun ~raise ->
  let protocol_version =
    Helpers.protocol_to_variant ~raise raw_options.protocol_version
  in
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~protocol_version ~raw_options ~syntax () in
  let Compiler_options.{ entry_point; _ } = options.frontend in
  let Compiler_options.{ views; _ } = options.backend in
  let michelson, views =
    Build.build_contract
      ~raise
      ~options
      entry_point
      views
      (Build.Source_input.From_file source_file)
  in
  let contract =
    Compile.Of_michelson.build_contract
      ~raise
      ~enable_typed_opt:options.backend.enable_typed_opt
      ~protocol_version
      michelson
      views
  in
  Compile.Of_michelson.measure ~raise contract


let list_declarations
    (raw_options : Raw_options.t)
    source_file
    display_format
    no_colour
    ()
  =
  format_result ~display_format ~no_colour Formatter.declarations_format
  @@ fun ~raise ->
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~raw_options ~syntax () in
  let prg =
    Build.qualified_typed ~raise Env ~options (Build.Source_input.From_file source_file)
  in
  let declarations = Compile.Of_typed.list_declarations raw_options.only_ep prg in
  source_file, declarations


let get_scope_raw
    (raw_options : Raw_options.t)
    (source_file : BuildSystem.Source_input.code_input)
    ~raise
    ()
  =
  let file_name =
    match source_file with
    | From_file file_name -> file_name
    | Raw { id; _ } -> id
    | Raw_input_lsp { file; _ } -> file
  in
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some file_name)
  in
  let protocol_version =
    Helpers.protocol_to_variant ~raise raw_options.protocol_version
  in
  let options = Compiler_options.make ~raw_options ~syntax ~protocol_version () in
  let Compiler_options.{ with_types; _ } = options.tools in
  let core_prg =
    (* While building [Build.qualified_core] we need a smaller AST (without stdlib)
       that is the reason for no_stdlib as true *)
    let options = Compiler_options.set_no_stdlib options true in
    match source_file with
    | From_file file_name ->
      Build.qualified_core ~raise ~options (Build.Source_input.From_file file_name)
    | Raw file -> Build.qualified_core_from_string ~raise ~options file
    | Raw_input_lsp { file; code } ->
      Build.qualified_core_from_raw_input ~raise ~options file code
  in
  let lib =
    (* We need stdlib for [Build.Stdlib.get], 
       because if [no_stdlib] we get [Build.Stdlib.empty] *)
    let options = Compiler_options.set_no_stdlib options false in
    Build.Stdlib.get ~options
  in
  let stdlib =
    Build.Stdlib.select_lib_typed syntax lib, Build.Stdlib.select_lib_core syntax lib
  in
  (* let () = assert (List.length (fst stdlib) = List.length (snd stdlib)) in *)
  Scopes.scopes ~raise ~options:options.middle_end ~with_types ~stdlib core_prg


let get_scope (raw_options : Raw_options.t) source_file display_format no_colour () =
  Scopes.Api_helper.format_result ~display_format ~no_colour
  @@ get_scope_raw raw_options (From_file source_file) ()


let get_scope_trace
    (raw_options : Raw_options.t)
    (source_file : BuildSystem.Source_input.code_input)
    ()
  =
  Trace.try_with
    ~fast_fail:false
    (fun ~raise ~catch ->
      let v = get_scope_raw raw_options source_file () ~raise in
      catch.errors (), catch.warnings (), Some v)
    (fun ~catch e -> e :: catch.errors (), catch.warnings (), None)
