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


let doc_to_string ~(width : int) (doc : PPrint.document) : string =
  let buffer = Buffer.create 131 in
  PPrint.ToBuffer.pretty 1.0 width buffer doc;
  Buffer.contents buffer


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
  doc_to_string ~width
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
