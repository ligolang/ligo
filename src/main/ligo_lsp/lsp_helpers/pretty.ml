open Imports
module Dialect_cst = Ligo_api.Dialect_cst

type pp_mode =
  { indent : int
  ; width : int
  }
[@@deriving show]

let with_pp_mode
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
  Helpers_pretty.doc_to_string ~width
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
        Simple_utils.Trace.to_stdlib_result
        @@ Nanopasses.decompile_sig_expr
             ~syntax
             (Simple_utils.Location.wrap ~loc:Simple_utils.Location.generated
             @@ Ast_core.S_sig core_sig)
      with
      | Ok (unified_sig_expr, _) -> Ok unified_sig_expr
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


let pretty_print_type_expression
    (* We try to decompile Ast_core to Ast_unified and then to CST.
     If this fails, we use the pretty printer for AST which gives nonpretty result *)
    :  ?prefix:
         PPrint.document
         (* In hovers we need to append things like `type t =` to type exprs,
            and since it should be done before [doc_to_string], such option is exposed here *)
    -> pp_mode -> syntax:Syntax_types.t -> Ast_core.type_expression -> pretty_print_result
  =
 fun ?prefix pp_mode ~syntax te ->
  let decompiled_cst_result
      : ( (Cst.Cameligo.type_expr, Cst.Jsligo.type_expr) Dialect_cst.dialect
      , [> `Exn of exn | `PassesError of Nanopasses.Errors.t ] ) result
    =
    try
      (* Both Ast_core -> Ast_unified and Ast_unified -> CST decompilers can throw exceptions :( *)
      match
        Simple_utils.Trace.to_stdlib_result @@ Nanopasses.decompile_ty_expr ~syntax te
      with
      | Error (err, _warnings) -> Error (`PassesError err)
      | Ok (s, _warnings) ->
        Ok
          (match syntax with
          | JsLIGO -> JsLIGO (Unification_jsligo.Decompile.decompile_type_expression s)
          | CameLIGO ->
            CameLIGO (Unification_cameligo.Decompile.decompile_type_expression s))
    with
    | exn -> Error (`Exn exn)
  in
  match decompiled_cst_result with
  | Ok cst ->
    let add_prefix = Option.value_map prefix ~default:Fun.id ~f:PPrint.( ^//^ ) in
    let print =
      Dialect_cst.
        { cameligo = add_prefix <@ uncurry Parsing.Cameligo.Pretty.print_type_expr
        ; jsligo = add_prefix <@ uncurry Parsing.Jsligo.Pretty.print_type_expr
        }
    in
    `Ok (with_pp_mode pp_mode print cst)
  | Error err ->
    `Nonpretty
      ( err
      , Helpers_pretty.doc_to_string ~width:10000
        @@
        let prefix_doc =
          Option.value_map prefix ~default:PPrint.empty ~f:PPrint.(fun x -> x ^^ space)
        in
        PPrint.(
          prefix_doc ^^ string (Format.asprintf "%a" Ast_core.PP.type_expression te)) )
