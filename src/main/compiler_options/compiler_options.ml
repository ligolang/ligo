open Environment

module Default_options = Raw_options.Default_options

type raw = Raw_options.raw

let make_raw_options = Raw_options.make

let default_raw_options = Raw_options.default

type formatter = {
  show_warnings : bool ;
  warning_as_error : bool ;
}

type frontend = {
  syntax : string ;
  dialect : string ;
  entry_point : string ;
  libraries : string list ;
  project_root : string option ;
}

type tools = {
  with_types : bool ;
  self_pass : bool ;
}

type test_framework = {
  steps : int ;
  generator : string ;
}

type middle_end = {
  infer : bool ;
  test : bool ;
  init_env : Environment.t ;
  protocol_version : Protocols.t ;
}

type backend = {
  protocol_version : Protocols.t ;
  disable_michelson_typechecking : bool ;
  without_run : bool ;
  views : string list ;
  constants : string list ;
  file_constants : string option ;
  has_env_comments : bool ; (* true if --michelson-comments env. if
                               true, empty seqs {} with comments will
                               not be erased during optimisation *)
}

type t = {
  formatter : formatter ;
  frontend : frontend ;
  tools : tools ;
  test_framework : test_framework ;
  middle_end : middle_end ;
  backend : backend ;
}

let make : 
  raw_options : raw ->
  ?protocol_version:Protocols.t ->
  ?test:bool ->
  ?has_env_comments : bool ->
  unit -> t =
  fun 
    ~raw_options
    ?(protocol_version = Protocols.current)
    ?(test = false)
    ?(has_env_comments = false)
    () ->
      let formatter = {
        show_warnings = raw_options.show_warnings;
        warning_as_error = raw_options.warning_as_error;
      } in
      let frontend = {
        syntax = raw_options.syntax ;
        dialect = raw_options.dialect ;
        libraries = raw_options.libraries;
        entry_point = raw_options.entry_point;
        project_root = raw_options.project_root;
      } in
      let tools = {
        with_types = raw_options.with_types;
        self_pass = raw_options.self_pass;
      } in
      let test_framework = {
        steps = raw_options.steps;
        generator = raw_options.generator;
      } in
      let middle_end = {
        infer = Default_options.infer ;
        test ;
        init_env = if test then default_with_test protocol_version else default protocol_version ;
        protocol_version;
      } in
      let backend = {
        protocol_version ;
        disable_michelson_typechecking = raw_options.disable_michelson_typechecking;
        without_run = raw_options.without_run;
        views = raw_options.views ;
        constants = raw_options.constants ;
        file_constants = raw_options.file_constants ;
        has_env_comments = has_env_comments ;
      } 
      in
      { 
        formatter ;
        frontend ;
        tools ;
        test_framework ;
        middle_end ;
        backend ;
      }

let set_init_env opts init_env = 
  { opts with middle_end = { opts.middle_end with init_env } }

let set_test_flag opts test =
  { opts with middle_end = { opts.middle_end with test } }

let set_entry_point opts entry_point =
  { opts with frontend = { opts.frontend with entry_point } }

let set_views opts views =
  { opts with backend = { opts.backend with views } }
