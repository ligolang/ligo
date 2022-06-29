let get_lib : Environment.Protocols.t -> Syntax_types.t -> test_enabled:bool -> string = fun protocol stx ~test_enabled ->
  let def str = "#define " ^ str ^ "\n" in
  let test_module = if test_enabled then def "TEST_LIB" else "" in
  let func_type =
    match stx with
    | ( PascaLIGO _ | ReasonLIGO | JsLIGO) -> def "UNCURRY"
    | CameLIGO                             -> def "CURRY"
  in
  let std = match protocol with
    | Environment.Protocols.Jakarta -> def "JAKARTA"
    | Environment.Protocols.Ithaca  -> def "ITHACA"
  in
  let lib = Ligo_lib.get () in
  test_module ^ func_type ^ std ^ lib

let stdlib ~options syntax =
  let lib = get_lib Compiler_options.(options.middle_end.protocol_version) syntax ~test_enabled:Compiler_options.(options.middle_end.test) in
  match Simple_utils.Trace.to_stdlib_result @@
          Ligo_compile.Utils.type_program_string ~add_warning:(fun _ -> ()) ~options CameLIGO lib with
  | Ok s -> s
  | Error e ->
     let error_msg = Format.asprintf "%a" (Main_errors.Formatter.error_ppformat ~display_format:Human_readable) e in
     failwith ("Error compiling the stdlib: " ^ error_msg)


let typed ~options (syntax : Syntax_types.t) =
  let open Helpers in
  let k = build_key ~options syntax in
  internalize_typed @@
    match LanguageMap.find_opt k @@ ! std_lib_cache with
    | None ->
       let typed, core = stdlib ~options syntax in
       std_lib_cache := LanguageMap.add k (typed, core) @@ !std_lib_cache;
       typed
    | Some (typed, _) -> typed

let core ~options (syntax : Syntax_types.t) =
  let open Helpers in
  let k = build_key ~options syntax in
  internalize_core @@
    match LanguageMap.find_opt k @@ ! std_lib_cache with
    | None ->
       let typed, core = stdlib ~options syntax in
       std_lib_cache := LanguageMap.add k (typed, core) @@ ! std_lib_cache;
       core
    | Some (_, core) -> core
