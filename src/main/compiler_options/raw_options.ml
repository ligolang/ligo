type t =
  { (* Formatter *)
    warning_as_error : bool
  ; no_colour : bool
  ; no_metadata_check : bool
  ; (* Supported features *)
    deprecated : bool
  ; (* Warnings *)
    warn_unused_rec : bool
  ; warn_infinite_loop : bool
  ; (* Frontend *)
    syntax : string
  ; entry_point : string list
  ; module_ : string
  ; libraries : string list
  ; project_root : string option
  ; transpiled : bool
  ; (* Tools *)
    with_types : bool
  ; self_pass : bool
  ; only_ep : bool
  ; skip_generated : bool
  ; (* Test framework *)
    test : bool
  ; steps : int
  ; generator : string
  ; cli_expr_inj : string option
  ; (* Middle-end *)
    no_stdlib : bool
  ; (* Backend *)
    protocol_version : string
  ; disable_michelson_typechecking : bool
  ; experimental_disable_optimizations_for_debugging : bool
  ; enable_typed_opt : bool
  ; without_run : bool
  ; views : string list
  ; constants : string list
  ; file_constants : string option
  }

module Default_options = struct
  (* Formatter *)
  let warning_as_error = false
  let no_colour = false
  let no_metadata_check = false

  (* Supported features *)
  let deprecated = false

  (* Warnings *)
  let warn_unused_rec = false
  let warn_infinite_loop = false

  (* Frontend *)
  let syntax = "auto"
  let entry_point = []
  let module_ = ""
  let libraries = []
  let project_root = None
  let transpiled = false

  (* Tools *)
  let only_ep = false
  let with_types = false
  let self_pass = false
  let skip_generated = false

  (* Test framework *)
  let test = false
  let steps = 1000000
  let generator = "random"
  let cli_expr_inj = None

  (* Middle-end *)
  let no_stdlib = false

  (* Backend *)
  let protocol_version = "current"
  let disable_michelson_typechecking = false
  let experimental_disable_optimizations_for_debugging = false
  let enable_typed_opt = false
  let without_run = false
  let views = []
  let constants = []
  let file_constants = None
end

let make
    ?(warning_as_error = Default_options.warning_as_error)
    ?(no_colour = Default_options.no_colour)
    ?(no_metadata_check = Default_options.no_metadata_check)
    ?(deprecated = Default_options.deprecated)
    ?(warn_unused_rec = Default_options.warn_unused_rec)
    ?(warn_infinite_loop = Default_options.warn_infinite_loop)
    ?(syntax = Default_options.syntax)
    ?(entry_point = Default_options.entry_point)
    ?(module_ = Default_options.module_)
    ?(libraries = Default_options.libraries)
    ?(project_root = Default_options.project_root)
    ?(transpiled = Default_options.transpiled)
    ?(only_ep = Default_options.only_ep)
    ?(with_types = Default_options.with_types)
    ?(self_pass = Default_options.self_pass)
    ?(skip_generated = Default_options.skip_generated)
    ?(test = Default_options.test)
    ?(steps = Default_options.steps)
    ?(generator = Default_options.generator)
    ?(cli_expr_inj = Default_options.cli_expr_inj)
    ?(protocol_version = Default_options.protocol_version)
    ?(no_stdlib = Default_options.no_stdlib)
    ?(disable_michelson_typechecking = Default_options.disable_michelson_typechecking)
    ?(experimental_disable_optimizations_for_debugging =
      Default_options.experimental_disable_optimizations_for_debugging)
    ?(enable_typed_opt = Default_options.enable_typed_opt)
    ?(without_run = Default_options.without_run)
    ?(views = Default_options.views)
    ?(constants = Default_options.constants)
    ?(file_constants = Default_options.file_constants)
    ()
  =
  { (* Formatter *)
    warning_as_error
  ; no_colour
  ; no_metadata_check
  ; (* Supported features *)
    deprecated
  ; (* Warnings *)
    warn_unused_rec
  ; warn_infinite_loop
  ; (* Frontend *)
    syntax
  ; entry_point
  ; module_
  ; libraries
  ; project_root
  ; transpiled
  ; (* Tools *)
    only_ep
  ; with_types
  ; self_pass
  ; skip_generated
  ; (* Test framework *)
    test
  ; steps
  ; generator
  ; cli_expr_inj
  ; (* Middle-end *)
    no_stdlib
  ; (* Backend *)
    protocol_version
  ; disable_michelson_typechecking
  ; experimental_disable_optimizations_for_debugging
  ; enable_typed_opt
  ; without_run
  ; views
  ; constants
  ; file_constants
  }
