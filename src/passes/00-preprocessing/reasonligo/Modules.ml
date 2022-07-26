let mk_module : string -> string -> string =
  fun file_name module_name ->
    Format.asprintf "module %s = %s" module_name file_name
