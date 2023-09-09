module Location = Simple_utils.Location

type t =
  { (* typed version of std_lib.mligo . Usage: repl (where the syntax is fixed) *)
    content_typed : Ast_typed.program
  ; content_core : Ast_core.program
  }

let empty =
  { content_typed = { pr_module = []; pr_sig = { sig_items = []; sig_sort = Ss_module } }
  ; content_core = []
  }


module Cache = struct
  (* LanguageMap are used to cache compilation of standard libs across :
    - multiple imports (#imports)
    - multiple compilation of contract in "ligo test"
  *)
  module LanguageMap = Simple_utils.Map.Make (struct
    type t = Environment.Protocols.t * bool

    let compare (pa, ta) (pb, tb) =
      Int.(abs (Environment.Protocols.compare pa pb) + abs (compare_bool ta tb))
  end)

  type cache = t LanguageMap.t

  let cache_ref = ref (LanguageMap.empty : cache)

  let build_key ~options =
    let open Compiler_options in
    options.middle_end.protocol_version, options.middle_end.test
end

let compile ~options x =
  let open Compiler_options in
  let no_colour : bool = options.tools.no_colour in
  match
    Simple_utils.Trace.to_stdlib_result
      (Ligo_compile.Utils.core_program_string ~options CameLIGO x)
  with
  | Ok (x, _w) -> Helpers.internalize_core x
  | Error (e, _w) ->
    let error_msg =
      Format.asprintf
        "%a"
        (Main_errors.Formatter.error_ppformat ~display_format:Human_readable ~no_colour)
        e
    in
    failwith ("Error compiling the stdlib: " ^ error_msg)


let type_ ~options x =
  let open Compiler_options in
  let no_colour = options.tools.no_colour in
  match
    Simple_utils.Trace.to_stdlib_result (Ligo_compile.Of_core.typecheck ~options x)
  with
  | Ok (x, _w) -> x
  | Error (e, _w) ->
    let error_msg =
      Format.asprintf
        "%a"
        (Main_errors.Formatter.error_ppformat ~display_format:Human_readable ~no_colour)
        e
    in
    failwith ("Error typing the stdlib: " ^ error_msg)


let get : options:Compiler_options.t -> unit -> t =
 fun ~options () ->
  let def str = "#define " ^ str ^ "\n" in
  let std =
    match options.middle_end.protocol_version with
    | Environment.Protocols.Mumbai -> def "MUMBAI"
    | Environment.Protocols.Nairobi -> def "NAIROBI"
  in
  let legacy_layout_tree =
    if Ligo_prim.Layout.legacy_layout_flag
    then def "LEGACY_LAYOUT_TREE"
    else "" in
  let lib = Ligo_lib.get () in
  let curry_content_core = compile ~options (std ^ legacy_layout_tree ^ lib) in
  let curry_content_typed = type_ ~options curry_content_core in
  { content_typed = curry_content_typed; content_core = curry_content_core }


let get ~options : t =
  if options.Compiler_options.middle_end.no_stdlib
  then empty
  else
    let open Cache in
    let k = build_key ~options in
    match LanguageMap.find_opt k @@ !cache_ref with
    | None ->
      let lib = get ~options () in
      cache_ref := LanguageMap.add k lib @@ !cache_ref;
      lib
    | Some typed -> typed


let select_lib_core (stx : Syntax_types.t) (lib : t) : Ast_core.program =
  match stx with
  | CameLIGO | JsLIGO | PascaLIGO -> lib.content_core


let select_lib_typed (stx : Syntax_types.t) (lib : t) : Ast_typed.program =
  match stx with
  | CameLIGO | JsLIGO | PascaLIGO -> lib.content_typed
