type raw = {
  (* Formatter *)
  show_warnings : bool ;
  warning_as_error : bool ;
  
  (* Frontend *)
  syntax : string ;
  dialect : string ;
  entry_point : string ;
  libraries : string list ;
  project_root : string option ;
  
  (* Tools *)
  with_types : bool ;
  self_pass : bool ;
  
  (* Test framework *)
  steps : int ;
  generator : string ;
  
  (* Backend *)
  protocol_version : string ;
  disable_michelson_typechecking : bool ;
  without_run : bool ;
  views : string list ;
  constants : string list ;
  file_constants : string option ;
}

module Default_options = struct 
  (* Formatter *)
  let show_warnings = true
  let warning_as_error = false
  
  (* Frontend *)
  let syntax = "auto"
  let dialect = "terse"
  let entry_point = "main"
  let libraries = []
  let project_root = None

  (* Tools *)
  let infer = false
  let with_types = false
  let self_pass = false
  
  (* Test framework *)
  let steps = 1000000
  let generator = "random"
  
  (* Backend *)
  let protocol_version = "current"
  let disable_michelson_typechecking = false
  let without_run = false
  let views = []
  let constants = []
  let file_constants = None
end

let make 
  ?(show_warnings = Default_options.show_warnings)
  ?(warning_as_error = Default_options.warning_as_error)
  ?(syntax = Default_options.syntax)
  ?(dialect = Default_options.dialect)
  ?(entry_point = Default_options.entry_point)
  ?(libraries = Default_options.libraries)
  ?(project_root = Default_options.project_root)
  ?(with_types = Default_options.with_types)
  ?(self_pass = Default_options.self_pass)
  ?(steps = Default_options.steps)
  ?(generator = Default_options.generator)
  ?(protocol_version = Default_options.protocol_version)
  ?(disable_michelson_typechecking = Default_options.disable_michelson_typechecking)
  ?(without_run = Default_options.without_run)
  ?(views = Default_options.views)
  ?(constants = Default_options.constants)
  ?(file_constants = Default_options.file_constants)
  () = 
{
  (* Formatter *)
  show_warnings ;
  warning_as_error ;
  
  (* Frontend *)
  syntax ;
  dialect ;
  entry_point ;
  libraries ;
  project_root ;
  
  (* Tools *)
  with_types ;
  self_pass ;
  
  (* Test framework *)
  steps ;
  generator ;
  
  (* Backend *)
  protocol_version ;
  disable_michelson_typechecking ;
  without_run ;
  views ;
  constants ;
  file_constants ;
}

let default =
{
  (* Formatter *)
  show_warnings = Default_options.show_warnings ;
  warning_as_error = Default_options.show_warnings ;
  
  (* Frontend *)
  syntax = Default_options.syntax ;
  dialect = Default_options.dialect ;
  entry_point = Default_options.entry_point ;
  libraries = Default_options.libraries ;
  project_root = Default_options.project_root ;
  
  (* Tools *)
  with_types = Default_options.with_types ;
  self_pass = Default_options.self_pass ;
  
  (* Test framework *)
  steps = Default_options.steps ;
  generator = Default_options.generator ;
  
  (* Backend *)
  protocol_version = Default_options.protocol_version ;
  disable_michelson_typechecking = Default_options.disable_michelson_typechecking ;
  without_run = Default_options.without_run ;
  views = Default_options.views ;
  constants = Default_options.constants ;
  file_constants = Default_options.file_constants ;
}
