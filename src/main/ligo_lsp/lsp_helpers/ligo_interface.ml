(* This module contains wrappers for various LIGO functions:
   get-scope that collects information about definitions in a file
   and pretty-printers that can print a CST as a code for some dialect or
   can decompile AST to CST and then print it *)
open Imports
module Get_scope = Get_scope
open Get_scope

type nonrec get_scope_info = get_scope_info

(** To support dirty files, we store some data about files in memory *)
type file_data =
  { syntax : Syntax_types.t
  ; code : string
  ; get_scope_info : get_scope_info
  }

let get_scope : deprecated:bool -> Path.t -> string -> get_scope_info =
 fun ~deprecated path source ->
  (* packages - project_root [later] *)
  let file = Path.to_string path in
  (* #include - Pass lib or dirs *)
  let dir_name = Filename.dirname file in
  (* FIXME [#1657]: Once we have a project system, set the correct [project_root]. *)
  let project_root = Some dir_name in
  let compiler_options =
    Compiler_options.Raw_options.make
      ~with_types:true
      ~libraries:[ dir_name ]
      ~deprecated
      ~project_root
      ()
  in
  unfold_get_scope
  @@ get_scope_trace compiler_options (Raw_input_lsp { file; code = source }) ()


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
      , unit * 'c
      , PPrint.document )
      Dialect_cst.from_dialect)
    (x : ('a, 'b, 'c) Dialect_cst.dialect)
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
  | PascaLIGO code ->
    (* XXX change type and use set_indent as well after adding ident to state for PascaLIGO *)
    pprint.pascaligo (Parsing.Pascaligo.Pretty.default_state, code)


let pretty_print_cst pp_mode ~(dialect_cst : Dialect_cst.t) : string =
  with_pp_mode
    pp_mode
    { cameligo = uncurry Parsing.Cameligo.Pretty.print
    ; jsligo = uncurry Parsing.Jsligo.Pretty.print
    ; pascaligo = uncurry Parsing.Pascaligo.Pretty.print
    }
    dialect_cst


let pretty_print_type_expression
    (* We try to decompile Ast_core to Ast_unified and then to CST.
     If this fails, we use the pretty printer for AST which gives nonpretty result *)
    :  ?prefix:
         PPrint.document
         (* In hovers we need to append things like `type t =` to type exprs,
            and since it should be done before [doc_to_string], such option is exposed here *)
    -> pp_mode -> syntax:Syntax_types.t -> Ast_core.type_expression
    -> [ `Ok of string
       | `Nonpretty of [ `Exn of exn | `PassesError of Passes.Errors.t ] * string
       ]
  =
 fun ?prefix pp_mode ~syntax te ->
  let decompiled_cst_result
      : ( ( Cst.Cameligo.type_expr
          , Cst.Jsligo.type_expr
          , Cst.Pascaligo.type_expr )
          Dialect_cst.dialect
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
            CameLIGO (Unification_cameligo.Decompile.decompile_type_expression s)
          | PascaLIGO ->
            PascaLIGO (Unification_pascaligo.Decompile.decompile_type_expression s))
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
        ; pascaligo = add_prefix <@ uncurry Parsing.Pascaligo.Pretty.print_type_expr
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
