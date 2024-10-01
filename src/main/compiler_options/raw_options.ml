type t =
  { (* Formatter *)
    warning_as_error : bool
  ; no_colour : bool
  ; no_metadata_check : bool
  ; (* Warnings *)
    warn_unused_rec : bool
  ; warn_infinite_loop : bool
  ; (* Frontend *)
    syntax : string
  ; module_ : string
  ; libraries : string list
  ; project_root : string option
  ; transpiled : bool
  ; parser_error_recovery : bool
  ; (* Tools *)
    with_types : bool
  ; self_pass : bool
  ; json_download : bool option
  ; only_ep : bool
  ; skip_generated : bool
  ; defs_only : bool
  ; typer_error_recovery : bool
  ; (* Test framework *)
    test : bool
  ; steps : int
  ; generator : string
  ; cli_expr_inj : string option
  ; (* Middle-end *)
    no_stdlib : bool
  ; array_as_list : bool
  ; (* Backend *)
    disable_michelson_typechecking : bool
  ; experimental_disable_optimizations_for_debugging : bool
  ; enable_typed_opt : bool
  ; without_run : bool
  ; constants : string list
  ; file_constants : string option
  ; function_body : bool
  ; lltz_ir : bool
  ; preprocess_define : string list
  }

module Default_options = struct
  (* Formatter *)
  let warning_as_error = false
  let no_colour = false
  let no_metadata_check = false

  (* Warnings *)
  let warn_unused_rec = false
  let warn_infinite_loop = false

  (* Frontend *)
  let syntax = "auto"
  let module_ = ""
  let libraries = []
  let project_root = None
  let transpiled = false
  let parser_error_recovery = false

  (* Tools *)
  let only_ep = false
  let with_types = false
  let self_pass = false
  let json_download = None
  let skip_generated = false
  let defs_only = false
  let typer_error_recovery = false

  (* Test framework *)
  let test = false
  let steps = 1000000
  let generator = "random"
  let cli_expr_inj = None

  (* Middle-end *)
  let no_stdlib = false
  let array_as_list = false

  (* Backend *)
  let disable_michelson_typechecking = false
  let experimental_disable_optimizations_for_debugging = false
  let enable_typed_opt = false
  let without_run = false
  let constants = []
  let file_constants = None
  let function_body = false
  let lltz_ir = false
  let preprocess_define = []
end

let make
    ?(warning_as_error = Default_options.warning_as_error)
    ?(no_colour = Default_options.no_colour)
    ?(no_metadata_check = Default_options.no_metadata_check)
    ?(json_download = Default_options.json_download)
    ?(warn_unused_rec = Default_options.warn_unused_rec)
    ?(warn_infinite_loop = Default_options.warn_infinite_loop)
    ?(syntax = Default_options.syntax)
    ?(module_ = Default_options.module_)
    ?(libraries = Default_options.libraries)
    ?(project_root = Default_options.project_root)
    ?(transpiled = Default_options.transpiled)
    ?(parser_error_recovery = Default_options.parser_error_recovery)
    ?(only_ep = Default_options.only_ep)
    ?(with_types = Default_options.with_types)
    ?(self_pass = Default_options.self_pass)
    ?(skip_generated = Default_options.skip_generated)
    ?(test = Default_options.test)
    ?(steps = Default_options.steps)
    ?(generator = Default_options.generator)
    ?(cli_expr_inj = Default_options.cli_expr_inj)
    ?(no_stdlib = Default_options.no_stdlib)
    ?(array_as_list = Default_options.array_as_list)
    ?(defs_only = Default_options.defs_only)
    ?(typer_error_recovery = Default_options.typer_error_recovery)
    ?(disable_michelson_typechecking = Default_options.disable_michelson_typechecking)
    ?(experimental_disable_optimizations_for_debugging =
      Default_options.experimental_disable_optimizations_for_debugging)
    ?(enable_typed_opt = Default_options.enable_typed_opt)
    ?(without_run = Default_options.without_run)
    ?(constants = Default_options.constants)
    ?(file_constants = Default_options.file_constants)
    ?(function_body = Default_options.function_body)
    ?(lltz_ir = Default_options.lltz_ir)
    ?(preprocess_define = Default_options.preprocess_define)
    ()
  =
  { (* Formatter *)
    warning_as_error
  ; no_colour
  ; no_metadata_check
  ; (* Warnings *)
    warn_unused_rec
  ; warn_infinite_loop
  ; (* Frontend *)
    syntax
  ; module_
  ; libraries
  ; project_root
  ; transpiled
  ; parser_error_recovery
  ; (* Tools *)
    only_ep
  ; with_types
  ; self_pass
  ; json_download
  ; skip_generated
  ; defs_only
  ; typer_error_recovery
  ; (* Test framework *)
    test
  ; steps
  ; generator
  ; cli_expr_inj
  ; (* Middle-end *)
    no_stdlib
  ; array_as_list
  ; (* Backend *)
    disable_michelson_typechecking
  ; experimental_disable_optimizations_for_debugging
  ; enable_typed_opt
  ; without_run
  ; constants
  ; file_constants
  ; function_body
  ; lltz_ir
  ; preprocess_define
  }
