let get_lib : Environment.Protocols.t -> Syntax_types.t -> string = fun protocol stx ->
  Scanf.unescaped @@
  match (protocol , stx) with
  | Environment.Protocols.Jakarta , ( PascaLIGO _ | ReasonLIGO | JsLIGO) -> Ligo_lib.Ligo_Stdlib.std_uncurry_jakarta
  | Environment.Protocols.Ithaca , ( PascaLIGO _ | ReasonLIGO | JsLIGO) -> Ligo_lib.Ligo_Stdlib.std_uncurry_ithaca
  | Environment.Protocols.Jakarta , CameLIGO -> Ligo_lib.Ligo_Stdlib.std_curry_jakarta
  | Environment.Protocols.Ithaca  , CameLIGO -> Ligo_lib.Ligo_Stdlib.std_curry_ithaca

let stdlib ~options syntax =
  let lib = get_lib Compiler_options.(options.middle_end.protocol_version) syntax in
  match Simple_utils.Trace.to_stdlib_result @@
          Ligo_compile.Utils.type_contract_string ~add_warning:(fun _ -> ()) ~options CameLIGO lib with
  | Ok s -> s
  | Error e ->
     let error_msg = Format.asprintf "%a" (Main_errors.Formatter.error_ppformat ~display_format:Human_readable) e in
     failwith ("Error compiling the stdlib: " ^ error_msg)

module LanguageMap = Simple_utils.Map.Make(struct type t = Syntax_types.t let compare = Syntax_types.compare end)
let cached = ref LanguageMap.empty

let typed ~options syntax =
  Helpers.internalize_typed @@
    match LanguageMap.find_opt syntax @@ ! cached with
    | None ->
       let typed, core = stdlib ~options syntax in
       cached := LanguageMap.add syntax (typed, core) @@ ! cached;
       typed
    | Some (typed, _) -> typed

let core ~options syntax =
  Helpers.internalize_core @@
    match LanguageMap.find_opt syntax @@ ! cached with
    | None ->
       let typed, core = stdlib ~options syntax in
       cached := LanguageMap.add syntax (typed, core) @@ ! cached;
       core
    | Some (_, core) -> core
