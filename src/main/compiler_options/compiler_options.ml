open Environment
module Default_options = Raw_options.Default_options
module Raw_options = Raw_options

(* TODO : Add a [common] section for options used in many stages,
   like the [no_colour] or the [syntax] options for example *)

type frontend =
  { syntax : Syntax_types.t option
  ; (* dialect : string ; [@dead "frontend.dialect"]  *)
    entry_point : string
  ; libraries : string list
  ; project_root : string option
  ; no_colour : bool
  }

type tools =
  { with_types : bool
  ; self_pass : bool
  ; no_colour : bool
  }

type test_framework =
  { steps : int
  ; cli_expr_inj : string option
  ; no_colour : bool
  }

type middle_end =
  { test : bool
  ; init_env : Environment.t
  ; protocol_version : Protocols.t
  ; warn_unused_rec : bool
  ; no_stdlib : bool
  ; syntax_for_errors : Syntax_types.t option
  ; no_colour : bool
  }

type backend =
  { protocol_version : Protocols.t
  ; disable_michelson_typechecking : bool
  ; experimental_disable_optimizations_for_debugging : bool
  ; enable_typed_opt : bool
  ; without_run : bool
  ; views : string list
  ; constants : string list
  ; file_constants : string option
  ; has_env_comments : bool
        (* true if --michelson-comments env. if
                               true, empty seqs {} with comments will
                               not be erased during optimisation *)
  ; no_colour : bool
  }

type common = { deprecated : bool }

type t =
  { frontend : frontend
  ; tools : tools
  ; test_framework : test_framework
  ; middle_end : middle_end
  ; backend : backend
  ; common : common
  }

let warn_unused_rec ~syntax should_warn =
  match syntax with
  | Some Syntax_types.JsLIGO -> false
  | Some CameLIGO | Some PascaLIGO | None -> should_warn


let make
    :  raw_options:Raw_options.t -> ?syntax:Syntax_types.t
    -> ?protocol_version:Protocols.t -> ?has_env_comments:bool -> unit -> t
  =
 fun ~raw_options
     ?syntax
     ?(protocol_version = Protocols.current)
     ?(has_env_comments = false)
     () ->
  let frontend =
    { syntax
    ; libraries = raw_options.libraries
    ; entry_point = raw_options.entry_point
    ; project_root = raw_options.project_root
    ; no_colour = raw_options.no_colour
    }
  in
  let tools =
    { with_types = raw_options.with_types
    ; self_pass = raw_options.self_pass
    ; no_colour = raw_options.no_colour
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
    ; init_env = default protocol_version
    ; protocol_version
    ; warn_unused_rec = warn_unused_rec ~syntax raw_options.warn_unused_rec
    ; no_stdlib = raw_options.no_stdlib
    ; syntax_for_errors = syntax
    ; no_colour = raw_options.no_colour
    }
  in
  let backend =
    { protocol_version
    ; disable_michelson_typechecking = raw_options.disable_michelson_typechecking
    ; experimental_disable_optimizations_for_debugging =
        raw_options.experimental_disable_optimizations_for_debugging
    ; enable_typed_opt = raw_options.enable_typed_opt
    ; without_run = raw_options.without_run
    ; views = raw_options.views
    ; constants = raw_options.constants
    ; file_constants = raw_options.file_constants
    ; has_env_comments
    ; no_colour = raw_options.no_colour
    }
  in
  let common = { deprecated = raw_options.deprecated } in
  { frontend; tools; test_framework; middle_end; backend; common }


let set_init_env opts init_env =
  { opts with middle_end = { opts.middle_end with init_env } }


let set_test_flag opts test = { opts with middle_end = { opts.middle_end with test } }

let set_entry_point opts entry_point =
  { opts with frontend = { opts.frontend with entry_point } }


let set_syntax opts syntax = { opts with frontend = { opts.frontend with syntax } }
let set_views opts views = { opts with backend = { opts.backend with views } }

let set_no_stdlib opts no_stdlib =
  { opts with middle_end = { opts.middle_end with no_stdlib } }
