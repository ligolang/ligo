open Core
open Handler
open Lsp_helpers
module CameLIGO_pretty = Parsing.Cameligo.Pretty
module JsLIGO_pretty = Parsing.Jsligo.Pretty

(* Currently we just select all toplevel cst nodes in given range and replace "sub-cst"
   by pretty printer result *)
(* TODO: format definitions from local modules, format subexpressions *)

type declaration =
  (Cst_cameligo.CST.declaration, Cst_jsligo.CST.statement) Dialect_cst.dialect

(** Returns the declaration range of the given declaration. *)
let decl_range : declaration -> Range.t =
  Range.of_region
  <@ Dialect_cst.from_dialect
       { cameligo = Cst_cameligo.CST.declaration_to_region
       ; jsligo = Cst_jsligo.CST.statement_to_region
       }

(** Collects all top-level declarations from the provided CST (does not visit inner
    modules). *)
let decls_of_cst : Dialect_cst.t -> declaration Nonempty_list.t =
  Dialect_cst.from_dialect
    { cameligo =
        Cst_cameligo.CST.(
          fun cst -> Nonempty_list.map ~f:(fun x -> Dialect_cst.CameLIGO x) cst.decl)
    ; jsligo =
        Cst_jsligo.CST.(
          fun (cst : t) ->
            Nonempty_list.map
              ~f:(fun (x, _) -> Dialect_cst.JsLIGO x)
              cst.statements (* Type inference is not working here *))
    }

(** Prints the provided declaration in the given syntax. *)
let print_decl : Pretty.pp_mode -> declaration -> string =
 fun pp_mode ->
  Pretty.with_pp_mode
    pp_mode
    { cameligo = uncurry CameLIGO_pretty.print_declaration
    ; jsligo = uncurry JsLIGO_pretty.print_statement
    }

(* [print_decl] produce a newline at the end of doc, which leads to a trailing newline
  inserted by range formatting in case we're not stripping it manually *)
let strip_trailing_newline (s : string) : string = String.rstrip s

(** Formats declarations whose ranges are completely inside the provided formatting range. *)
let range_formatting
    (pp_mode : Pretty.pp_mode)
    (decls : declaration Nonempty_list.t)
    (range : Range.t)
    : TextEdit.t list option
  =
  let f decl = Range.inside ~small:(decl_range decl) ~big:range in
  match List.filter ~f @@ Nonempty_list.to_list decls with
  | [] -> None
  | d :: ds as declarations_in_range ->
    (* We should create one TextEdit instead of multiple (i.e. one for
       each declaration) because we want to have exactly one empty
       line between pretty-printed declarations, so range formatting
       with [whole_file_range] is equivalent to formatting *)
    let covering_interval =
      Range.cover_nseq (Nonempty_list.map ~f:decl_range (d :: ds))
    in
    let content =
      declarations_in_range
      |> List.map ~f:(print_decl pp_mode)
      |> String.concat ~sep:"\n"
      |> strip_trailing_newline
    in
    Some [ TextEdit.create ~newText:content ~range:covering_interval ]

(* FIXME #1765: use tab size from FormattingOptions *)

(** Runs the handler for the document formatting request. This is normally invoked when
    the user clicks the "Format Selection" button. *)
let on_req_range_formatting
    : Path.t -> Range.t -> FormattingOptions.t -> TextEdit.t list option Handler.t
  =
 fun file range opts ->
  let open Handler.Let_syntax in
  let%bind pp_mode = Formatting.get_pp_mode file opts in
  let%bind () =
    send_debug_msg
    @@ Format.asprintf
         "Formatting request on %s, mode: %a"
         (Path.to_string file)
         Pretty.pp_pp_mode
         pp_mode
  in
  if Helpers_file.is_packaged @@ Path.to_string file
  then (
    let%bind () =
      send_message ~type_:Error @@ "Can not format a file from an imported package."
    in
    return None)
  else (
    let on_error _err =
      send_message ~type_:Error
      @@ "Can not apply range formatting on a file with syntax errors"
    in
    with_cst ~strict:true ~on_error file ~default:None
    @@ fun cst ->
    let edits = range_formatting pp_mode (decls_of_cst cst) range in
    let%bind () =
      when_
        (Option.is_none edits)
        (send_message ~type_:Warning
        @@ "Range formatting: currently can format only toplevel declarations, none \
            selected by given range")
    in
    let%bind () =
      when_some_ edits
      @@ fun edits_list ->
      send_debug_msg
      @@ "Range formatting: returned replace for ranges "
      ^ String.concat
          ~sep:", "
          (List.map ~f:(fun x -> Range.to_string @@ x.range) edits_list)
    in
    return edits)
