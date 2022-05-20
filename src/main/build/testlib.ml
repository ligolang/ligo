let get_lib : stub:bool -> Syntax_types.t -> string = fun ~stub stx ->
  let stdlib = Stdlib.get_lib Environment.Protocols.in_use CameLIGO in
  let testlib = match (stub , stx) with
    | false , ( PascaLIGO _ | ReasonLIGO | JsLIGO) -> Ligo_lib.Ligo_Testlib.test_uncurry
    | false , ( CameLIGO ) -> Ligo_lib.Ligo_Testlib.test_curry
    | true , ( PascaLIGO _ | ReasonLIGO | JsLIGO) -> Ligo_lib.Ligo_Testlib.test_uncurry_stub
    | true , ( CameLIGO ) -> Ligo_lib.Ligo_Testlib.test_curry_stub in
  stdlib ^ "\n" ^ testlib

let testlib ~options syntax =
  if options.Compiler_options.middle_end.test then
    match Simple_utils.Trace.to_stdlib_result @@
            Ligo_compile.Utils.type_program_string ~add_warning:(fun _ -> ()) ~options CameLIGO (get_lib ~stub:false syntax) with
    | Ok s -> s
    | Error e ->
       let error_msg = Format.asprintf "%a" (Main_errors.Formatter.error_ppformat ~display_format:Human_readable) e in
       failwith ("Error compiling the testlib: " ^ error_msg)
  else
    match Simple_utils.Trace.to_stdlib_result @@
            Ligo_compile.Utils.type_program_string ~add_warning:(fun _ -> ()) ~options CameLIGO (get_lib ~stub:true syntax) with
    | Ok s -> s
    | Error e ->
       let error_msg = Format.asprintf "%a" (Main_errors.Formatter.error_ppformat ~display_format:Human_readable) e in
       failwith ("Error compiling the testlib: " ^ error_msg)

let typed ~options syntax =
  let open Helpers in
  let k = build_key ~options syntax in
  internalize_typed @@
    match LanguageMap.find_opt k @@ ! test_lib_cache with
    | None ->
       let typed, core = testlib ~options syntax in
       test_lib_cache := LanguageMap.add k (typed, core) @@ ! test_lib_cache;
       typed
    | Some (typed, _) -> typed

let core ~options syntax =
  let open Helpers in
  let k = build_key ~options syntax in
  internalize_core @@
    match LanguageMap.find_opt k @@ ! test_lib_cache with
    | None ->
       let typed, core = testlib ~options syntax in
       test_lib_cache := LanguageMap.add k (typed, core) @@ ! test_lib_cache;
       core
    | Some (_, core) -> core
