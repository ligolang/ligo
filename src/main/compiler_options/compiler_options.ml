module Default_options = Raw_options.Default_options
module Raw_options = Raw_options

(* TODO : Add a [common] section for options used in many stages,
   like the [no_colour] or the [syntax] options for example *)

type frontend =
  { syntax : Syntax_types.t option
  ; module_ : string
  ; libraries : string list
  ; project_root : string option
  ; transpiled : bool
  ; warn_infinite_loop : bool
  ; preprocess_define : string list
  }

type tools =
  { with_types : bool
  ; self_pass : bool
  ; no_colour : bool
  ; json_download : bool option
  }

type test_framework =
  { steps : int
  ; cli_expr_inj : string option
  ; no_colour : bool
  }

type middle_end =
  { test : bool
  ; warn_unused_rec : bool
  ; no_stdlib : bool
  ; syntax_for_errors : Syntax_types.t option
  ; no_colour : bool
  ; no_metadata_check : bool
  ; typer_error_recovery : bool
  ; array_as_list : bool
  }

type backend =
  { disable_michelson_typechecking : bool
  ; experimental_disable_optimizations_for_debugging : bool
  ; enable_typed_opt : bool
  ; without_run : bool
  ; constants : string list
  ; file_constants : string option
  ; has_env_comments : bool
        (* true if --michelson-comments env. if
                               true, empty seqs {} with comments will
                               not be erased during optimisation *)
  ; no_colour : bool
  ; function_body : bool
  }

type t =
  { frontend : frontend
  ; tools : tools
  ; test_framework : test_framework
  ; middle_end : middle_end
  ; backend : backend
  }

let warn_unused_rec ~syntax should_warn =
  match syntax with
  | Some Syntax_types.JsLIGO -> false
  | Some CameLIGO | None -> should_warn


let make
    :  raw_options:Raw_options.t -> ?syntax:Syntax_types.t -> ?has_env_comments:bool
    -> unit -> t
  =
 fun ~raw_options ?syntax ?(has_env_comments = false) () ->
  let frontend =
    { syntax
    ; libraries = raw_options.libraries
    ; module_ = raw_options.module_
    ; project_root = raw_options.project_root
    ; transpiled = raw_options.transpiled
    ; warn_infinite_loop = raw_options.warn_infinite_loop
    ; preprocess_define = raw_options.preprocess_define
    }
  in
  let tools =
    { with_types = raw_options.with_types
    ; self_pass = raw_options.self_pass
    ; no_colour = raw_options.no_colour
    ; json_download = raw_options.json_download
    }
  in
  let test_framework =
    { steps = raw_options.steps
    ; cli_expr_inj = raw_options.cli_expr_inj
    ; no_colour = raw_options.no_colour
    }
  in
  let middle_end =
    { test = raw_options.test
    ; warn_unused_rec = warn_unused_rec ~syntax raw_options.warn_unused_rec
    ; no_stdlib = raw_options.no_stdlib
    ; array_as_list = raw_options.array_as_list
    ; syntax_for_errors = syntax
    ; no_colour = raw_options.no_colour
    ; no_metadata_check = raw_options.no_metadata_check
    ; typer_error_recovery = raw_options.typer_error_recovery
    }
  in
  let backend =
    { disable_michelson_typechecking = raw_options.disable_michelson_typechecking
    ; experimental_disable_optimizations_for_debugging =
        raw_options.experimental_disable_optimizations_for_debugging
    ; enable_typed_opt = raw_options.enable_typed_opt
    ; without_run = raw_options.without_run
    ; constants = raw_options.constants
    ; file_constants = raw_options.file_constants
    ; has_env_comments
    ; no_colour = raw_options.no_colour
    ; function_body = raw_options.function_body
    }
  in
  { frontend; tools; test_framework; middle_end; backend }


let set_test_flag opts test = { opts with middle_end = { opts.middle_end with test } }
let set_syntax opts syntax = { opts with frontend = { opts.frontend with syntax } }

let set_no_stdlib opts no_stdlib =
  { opts with middle_end = { opts.middle_end with no_stdlib } }
