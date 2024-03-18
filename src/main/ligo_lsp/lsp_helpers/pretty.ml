open Imports
module Dialect_cst = Ligo_api.Dialect_cst

type pp_mode =
  { indent : int
  ; width : int
  }
[@@deriving show]

let with_pp_mode
    ?(doc_to_string = Helpers_pretty.doc_to_string)
    { indent; width }
    (pprint :
      (* Intentionally use explicit state so type error could occur
      if we'll add some param to only one pretty printer and we'll think
      about changing the pp_mode to (Parsing_shared.PrettyComb.state,...) dialect  *)
      ( Parsing_shared.PrettyComb.state * 'a
      , Parsing_shared.PrettyComb.state * 'b
      , PPrint.document )
      Dialect_cst.from_dialect)
    (x : ('a, 'b) Dialect_cst.dialect)
  =
  let set_ident pprint_state =
    object
      method indent = indent
      method leading_vbar = pprint_state#leading_vbar
    end
  in
  doc_to_string ~width
  @@
  match x with
  | CameLIGO code ->
    (* FIXME #1923 should set_ident here but CameLIGO gives nonpretty result with custom ident *)
    pprint.cameligo (Parsing.Cameligo.Pretty.default_state, code)
  | JsLIGO code -> pprint.jsligo (set_ident Parsing.Jsligo.Pretty.default_state, code)


let pretty_print_cst pp_mode ~(dialect_cst : Dialect_cst.t) : string =
  with_pp_mode
    pp_mode
    { cameligo = uncurry Parsing.Cameligo.Pretty.print
    ; jsligo = uncurry Parsing.Jsligo.Pretty.print
    }
    dialect_cst


type pretty_print_result =
  [ `Ok of string
  | `Nonpretty of [ `Exn of exn | `PassesError of Passes.Errors.t ] * string
  ]

let pretty_print_signature
    : syntax:Syntax_types.t -> Ast_core.signature -> pretty_print_result
  =
 fun ~syntax core_sig ->
  let unified_sig_result
      : (Ast_unified.sig_expr, [> `Exn of exn | `PassesError of Passes.Errors.t ]) result
    =
    try
      match
        Simple_utils.Trace.to_stdlib_result ~fast_fail:Fast_fail
        @@ Nanopasses.decompile_sig_expr
             ~syntax
             (Simple_utils.Location.wrap ~loc:Simple_utils.Location.generated
             @@ Ast_core.S_sig core_sig)
      with
      | Ok (unified_sig_expr, (), _) -> Ok unified_sig_expr
      | Error (err, _) -> Error (`PassesError err)
    with
    | exn -> Error (`Exn exn)
  in
  match unified_sig_result with
  | Ok unified_sig ->
    let to_syntax =
      match syntax with
      | CameLIGO ->
        let open Parsing.Cameligo in
        Buffer.contents
        <@ pretty_print_signature_expr Parsing.Cameligo.Pretty.default_state
        <@ Unification.Cameligo.decompile_sig_expr
      | JsLIGO ->
        let open Parsing.Jsligo in
        Buffer.contents
        <@ pretty_print_signature_expr Parsing.Cameligo.Pretty.default_state
        <@ Unification.Jsligo.decompile_sig_expr
    in
    `Ok (to_syntax unified_sig)
  | Error err ->
    `Nonpretty
      ( err
      , Helpers_pretty.doc_to_string ~width:10000
        @@ PPrint.(string (Format.asprintf "%a" Ast_core.PP.signature core_sig)) )


let decompile_type
    :  syntax:Syntax_types.t -> Ast_core.ty_expr
    -> ( (Cst.Cameligo.type_expr, Cst.Jsligo.type_expr) Dialect_cst.dialect
       , [> `Exn of exn | `PassesError of Nanopasses.Errors.t ] )
       result
  =
 fun ~syntax te ->
  try
    (* Both Ast_core -> Ast_unified and Ast_unified -> CST decompilers can throw exceptions :( *)
    match
      Simple_utils.Trace.to_stdlib_result ~fast_fail:Fast_fail
      @@ Nanopasses.decompile_ty_expr ~syntax te
    with
    | Error (err, _warnings) -> Error (`PassesError err)
    | Ok (s, (), _warnings) ->
      Ok
        (match syntax with
        | JsLIGO -> JsLIGO (Unification_jsligo.Decompile.decompile_type_expression s)
        | CameLIGO ->
          CameLIGO (Unification_cameligo.Decompile.decompile_type_expression s))
  with
  | exn -> Error (`Exn exn)


let pretty_print_variant
    :  pp_mode -> syntax:Syntax_types.t -> Ligo_prim.Label.t * Ast_core.ty_expr
    -> pretty_print_result
  =
 fun pp_mode ~syntax (label, typ) ->
  let decompiled_cst_result =
    (* Let's handle generated unit type *)
    if Loc.is_dummy_or_generated typ.location
    then (
      match syntax with
      | CameLIGO -> Ok (Dialect_cst.CameLIGO None)
      | JsLIGO -> Ok (JsLIGO None))
    else (
      match decompile_type ~syntax typ with
      | Ok (CameLIGO typ) -> Ok (CameLIGO (Some typ))
      | Ok (JsLIGO typ) -> Ok (JsLIGO (Some typ))
      | Error err -> Error err)
  in
  match decompiled_cst_result with
  | Ok cst ->
    let print =
      let print_cameligo (state, te) =
        let variant =
          Region.wrap_ghost
          @@ Unification_cameligo.Decompile.decompile_variant label te []
        in
        Parsing.Cameligo.Pretty.(print_variant state variant)
      in
      let print_jsligo (state, te) =
        let variant =
          Region.wrap_ghost @@ Unification_jsligo.Decompile.decompile_variant label te []
        in
        Parsing.Jsligo.Pretty.(print_legacy_variant print_type_expr state variant)
      in
      Dialect_cst.{ cameligo = print_cameligo; jsligo = print_jsligo }
    in
    `Ok (with_pp_mode pp_mode print cst)
  | Error err ->
    `Nonpretty
      ( err
      , Helpers_pretty.doc_to_string ~width:10000
        @@
        let (Label (label, _)) = label in
        let prefix_doc = PPrint.(string label ^//^ string "->") in
        PPrint.(
          prefix_doc ^//^ string (Format.asprintf "%a" Ast_core.PP.type_expression typ))
      )


let pretty_print_type_expression
    (* We try to decompile Ast_core to Ast_unified and then to CST.
     If this fails, we use the pretty printer for AST which gives nonpretty result *)
    :  ?prefix:
         PPrint.document
         (* In hovers we need to append things like `type t =` to type exprs,
            and since it should be done before [doc_to_string], such option is exposed here *)
    -> ?doc_to_string:(width:int -> PPrint.document -> string) -> pp_mode
    -> syntax:Syntax_types.t -> Ast_core.type_expression -> pretty_print_result
  =
 fun ?prefix ?(doc_to_string = Helpers_pretty.doc_to_string) pp_mode ~syntax te ->
  match decompile_type ~syntax te with
  | Ok cst ->
    let add_prefix = Option.value_map prefix ~default:Fun.id ~f:PPrint.( ^//^ ) in
    let print =
      Dialect_cst.
        { cameligo = add_prefix <@ uncurry Parsing.Cameligo.Pretty.print_type_expr
        ; jsligo = add_prefix <@ uncurry Parsing.Jsligo.Pretty.print_type_expr
        }
    in
    `Ok (with_pp_mode ~doc_to_string pp_mode print cst)
  | Error err ->
    `Nonpretty
      ( err
      , doc_to_string ~width:10000
        @@
        let prefix_doc =
          Option.value_map prefix ~default:PPrint.empty ~f:PPrint.(fun x -> x ^^ space)
        in
        PPrint.(
          prefix_doc ^^ string (Format.asprintf "%a" Ast_core.PP.type_expression te)) )


let show_type
    :  syntax:Syntax_types.t -> ?doc_to_string:(width:int -> PPrint.document -> string)
    -> Ast_core.type_expression -> string
  =
  (* VSCode is ignoring any newlines in completion detail *)
  let pp_mode = { width = 60; indent = 2 } in
  fun ~syntax ?(doc_to_string = Helpers_pretty.doc_to_string) te ->
    match pretty_print_type_expression ~doc_to_string pp_mode ~syntax te with
    | `Ok str -> str
    (* Sending log messages from here or adding exn to return type will make the code less
       straightforward, so we're just silently ignoring it since one can use hover on this
       term to see the exn anyway. *)
    | `Nonpretty (_exn, str) -> str
