let get_lib : Environment.Protocols.t -> Syntax_types.t -> string = fun protocol stx ->
  match (protocol , stx) with
  | Environment.Protocols.Jakarta , ( PascaLIGO _ | ReasonLIGO | JsLIGO) -> Ligo_lib.Ligo_Stdlib.std_uncurry_jakarta
  | Environment.Protocols.Ithaca , ( PascaLIGO _ | ReasonLIGO | JsLIGO) -> Ligo_lib.Ligo_Stdlib.std_uncurry_ithaca
  | Environment.Protocols.Jakarta , CameLIGO -> Ligo_lib.Ligo_Stdlib.std_curry_jakarta
  | Environment.Protocols.Ithaca  , CameLIGO -> Ligo_lib.Ligo_Stdlib.std_curry_ithaca

let stdlib ~options syntax =
  let lib = get_lib Compiler_options.(options.middle_end.protocol_version) syntax in
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
