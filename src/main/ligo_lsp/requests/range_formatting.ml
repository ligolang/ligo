open Linol_lwt
open Handler
open Utils
module Region = Simple_utils.Region
module Nseq = Simple_utils.Utils
module CameLIGO_pretty = Parsing.Cameligo.Pretty
module PascaLIGO_pretty = Parsing.Pascaligo.Pretty
module JsLIGO_pretty = Parsing.Jsligo.Pretty

(* Currently we just select all toplevel cst nodes in given range and replace "sub-cst"
   by pretty printer result *)
(* FIXME: add a configuration field for width to formatting, use it here too *)
(* TODO: format definitions from local modules, format subexpressions *)

type 'decl range_formatting_mode =
  { range_of_decl : 'decl -> Range.t
  ; print_decl : 'decl -> PPrint.document
  }

(* [print_decl] produce a newline at the end of doc, which leads to a trailing newline
  inserted by range formatting in case we're not stripping it manually *)
let strip_trailing_newline (s : string) : string = String.rstrip s

let range_formatting
    ({ range_of_decl; print_decl } : 'decl range_formatting_mode)
    (decls : 'decl Nseq.nseq)
    (range : Range.t)
    : TextEdit.t list option
  =
  let f decl = range_inside ~small:(range_of_decl decl) ~big:range in
  match List.filter ~f @@ Nseq.nseq_to_list decls with
  | [] -> None
  | d :: ds as declarations_in_range ->
    (* We should create one TextEdit instead of multiple (i.e. one for each declaration)
       because we want to have exactly one empty line between pretty-printed declarations,
       so range formatting with [whole_file_range] is equivalent to formatting *)
    let covering_interval = range_cover_nseq (Nseq.nseq_map range_of_decl (d, ds)) in
    let content =
      let open PPrint in
      declarations_in_range
      |> List.map ~f:print_decl
      |> separate_map hardline group
      |> Ligo_interface.doc_to_string ~width:80
      |> strip_trailing_newline
    in
    Some [ TextEdit.create ~newText:content ~range:covering_interval ]


let on_req_range_formatting : DocumentUri.t -> Range.t -> TextEdit.t list option Handler.t
  =
 fun uri range ->
  let@ () = send_debug_msg @@ "Formatting request on " ^ DocumentUri.to_path uri in
  let on_error _err =
    send_message ~type_:Error
    @@ "Can not apply range formatting on a file with syntax errors"
  in
  with_cst ~strict:true ~on_error uri None
  @@ fun cst ->
  let@ edits =
    return
    @@
    match cst with
    | CameLIGO_cst cst ->
      range_formatting
        { range_of_decl = region_to_range @. Cst_cameligo.CST.declaration_to_region
        ; print_decl =
            CameLIGO_pretty.print_declaration CameLIGO_pretty.default_environment
        }
        cst.decl
        range
    | PascaLIGO_cst cst ->
      range_formatting
        { range_of_decl = region_to_range @. Cst_pascaligo.CST.region_of_S_Decl
        ; print_decl =
            PascaLIGO_pretty.print_declaration PascaLIGO_pretty.default_environment
        }
        cst.decl
        range
    | JsLIGO_cst cst ->
      range_formatting
        { range_of_decl = region_to_range @. Cst_jsligo.CST.toplevel_statement_to_region
        ; print_decl =
            JsLIGO_pretty.print_toplevel_statement JsLIGO_pretty.default_environment
        }
        cst.statements
        range
  in
  let@ () =
    when_
      (Option.is_none edits)
      (send_message ~type_:Warning
      @@ "Range formatting: currently can format only toplevel declarations, none \
          selected by given range")
  in
  let@ () =
    when_some_ edits
    @@ fun edits_list ->
    send_debug_msg
    @@ "Range formatting: returned replace for ranges "
    ^ String.concat
        ~sep:", "
        (List.map ~f:(fun x -> range_to_string @@ x.range) edits_list)
  in
  return edits
