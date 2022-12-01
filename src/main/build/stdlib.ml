type t =
  { curry : lib
  ; uncurry : lib
        (* typed and untyped version of both curry and uncurry libraries wrapped into a module:
   module Curry = struct
     let x = <..>
     let y = <..>
   end
   module Uncurry = struct
    let x = <..>
    let y = <..>
   end
*)
  ; typed_mod_def : Ast_typed.program
  ; core_mod_def : Ast_core.program
  }

and lib =
  { (* preludes to be prepended X_mod_def's at top-level and within imported modules *)
    prelude_core : Ast_core.program
  ; prelude_typed : Ast_typed.program
        (* typed version of std_lib.mligo . Usage: repl (where the syntax is fixed) *)
  ; content_typed : Ast_typed.program
  }

let empty =
  let e = { prelude_core = []; prelude_typed = []; content_typed = [] } in
  { curry = e; uncurry = e; typed_mod_def = []; core_mod_def = [] }


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
  match
    Simple_utils.Trace.to_stdlib_result
      (Ligo_compile.Utils.core_program_string ~options CameLIGO x)
  with
  | Ok (x, _w) -> Helpers.internalize_core x
  | Error (e, _w) ->
    let error_msg =
      Format.asprintf
        "%a"
        (Main_errors.Formatter.error_ppformat ~display_format:Human_readable)
        e
    in
    failwith ("Error compiling the stdlib: " ^ error_msg)


let type_ ~options x =
  match
    Simple_utils.Trace.to_stdlib_result (Ligo_compile.Of_core.typecheck ~options Env x)
  with
  | Ok (x, _w) -> x
  | Error (e, _w) ->
    let error_msg =
      Format.asprintf
        "%a"
        (Main_errors.Formatter.error_ppformat ~display_format:Human_readable)
        e
    in
    failwith ("Error typing the stdlib: " ^ error_msg)


let get : options:Compiler_options.t -> unit -> t =
 fun ~options () ->
  let def str = "#define " ^ str ^ "\n" in
  let std =
    match options.middle_end.protocol_version with
    | Environment.Protocols.Lima -> def "LIMA"
    | Environment.Protocols.Kathmandu -> def "KATHMANDU"
  in
  let lib = Ligo_lib.get () in
  let binder_curry = Ligo_prim.Module_var.fresh ~name:"Curry_lib" () in
  let binder_uncurry = Ligo_prim.Module_var.fresh ~name:"Uncurry_lib" () in
  let curry_content_core = compile ~options (def "CURRY" ^ std ^ lib) in
  let curry_content_typed = type_ ~options curry_content_core in
  let uncurry_content_core = compile ~options (def "UNCURRY" ^ std ^ lib) in
  let uncurry_content_typed = type_ ~options uncurry_content_core in
  (* TODO: sanity check ? curry_content_typed and uncurry_content_typed should have the same signature modulo curry/uncurry style *)
  let typed_mod_def =
    let open Ligo_prim.Module_expr in
    let open Ast_typed in
    let module_attr = Ast_typed.TypeOrModuleAttr.{ public = true; hidden = true } in
    [ Location.wrap
      @@ D_module
           { module_binder = binder_curry
           ; module_attr
           ; module_ = Location.wrap @@ M_struct curry_content_typed
           }
    ; Location.wrap
      @@ D_module
           { module_binder = binder_uncurry
           ; module_attr
           ; module_ = Location.wrap @@ M_struct uncurry_content_typed
           }
    ]
  in
  let core_mod_def =
    let open Ligo_prim.Module_expr in
    let open Ast_core in
    let module_attr = Ast_core.TypeOrModuleAttr.{ public = true; hidden = true } in
    [ Location.wrap
      @@ D_module
           { module_binder = binder_curry
           ; module_attr
           ; module_ = Location.wrap @@ M_struct curry_content_core
           }
    ; Location.wrap
      @@ D_module
           { module_binder = binder_uncurry
           ; module_attr
           ; module_ = Location.wrap @@ M_struct uncurry_content_core
           }
    ]
  in
  let uncurry =
    let prelude_core = Helpers.get_aliases_prelude binder_uncurry uncurry_content_typed in
    let options =
      Compiler_options.set_init_env
        options
        (Environment.append options.middle_end.init_env typed_mod_def)
    in
    let prelude_typed = type_ ~options prelude_core in
    { prelude_core; prelude_typed; content_typed = uncurry_content_typed }
  in
  let curry =
    let prelude_core = Helpers.get_aliases_prelude binder_curry curry_content_typed in
    let options =
      Compiler_options.set_init_env
        options
        (Environment.append options.middle_end.init_env typed_mod_def)
    in
    let prelude_typed = type_ ~options prelude_core in
    { prelude_core; prelude_typed; content_typed = curry_content_typed }
  in
  { curry; uncurry; typed_mod_def; core_mod_def }


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


let select_prelude_core (stx : Syntax_types.t) (lib : t) : Ast_core.program =
  match stx with
  | CameLIGO -> lib.curry.prelude_core
  | PascaLIGO | JsLIGO | ReasonLIGO -> lib.uncurry.prelude_core


let select_prelude_typed (stx : Syntax_types.t) (lib : t) : Ast_typed.program =
  match stx with
  | CameLIGO -> lib.curry.prelude_typed
  | PascaLIGO | JsLIGO | ReasonLIGO -> lib.uncurry.prelude_typed


let select_lib_typed (stx : Syntax_types.t) (lib : t) : Ast_typed.program =
  match stx with
  | CameLIGO -> lib.curry.content_typed
  | PascaLIGO | JsLIGO | ReasonLIGO -> lib.uncurry.content_typed
