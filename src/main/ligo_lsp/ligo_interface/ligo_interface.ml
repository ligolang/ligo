module Get_scope = Get_scope

open Get_scope
open Utils
open Linol_lwt

type nonrec get_scope_info = get_scope_info

module CameLIGO_pretty =
  Parsing_shared.Common.MakePretty (Cst_cameligo.CST) (Parsing_cameligo.Pretty)

module JsLIGO_pretty =
  Parsing_shared.Common.MakePretty (Cst_jsligo.CST) (Parsing_jsligo.Pretty)

module PascaLIGO_pretty =
  Parsing_shared.Common.MakePretty (Cst_pascaligo.CST) (Parsing_pascaligo.Pretty)

(** To support dirty files, we store some data about files in memory *)
type file_data =
  { syntax : Syntax_types.t
  ; code : string
  ; get_scope_info : get_scope_info
  }

let get_scope : DocumentUri.t -> string -> get_scope_info =
 fun uri source ->
  (* packages - project_root [later] *)
  let file = DocumentUri.to_path uri in
  (* #include - Pass lib or dirs *)
  let dir_name = Filename.dirname file in
  let compiler_options =
    Compiler_options.Raw_options.make
      ~with_types:true
      ~libraries:[ dir_name ]
      ~deprecated:true
      ()
  in
  unfold_get_scope
  @@ get_scope_trace compiler_options (Raw_input_lsp { file; code = source }) ()


let doc_to_string ~(width : int) (doc : PPrint.document) : string =
  let buffer = Buffer.create 131 in
  PPrint.ToBuffer.pretty 1.0 width buffer doc;
  Buffer.contents buffer


let pretty_print_cst ~(width : int) ~(dialect_cst : dialect_cst) : string =
  let doc =
    match dialect_cst with
    | CameLIGO_cst cst -> Parsing_cameligo.Pretty.print cst
    | JsLIGO_cst cst -> Parsing_jsligo.Pretty.print cst
    | PascaLIGO_cst cst -> Parsing_pascaligo.Pretty.print cst
  in
  doc_to_string ~width doc
