open Core
open Common
open Lsp_helpers

(** Scans the file for [#include] and [#import] directives and provides file completions
    for the provided [pos]ition in the [current_file], if it's over such a directive.
    [code] is the source file of the document (this function uses regexes and not the
    CST). The [project_root] is used to scan for LIGO files from the project root, and the
    mod res is used to provide paths to LIGO registry packages. *)
let get_files_for_completions
    ~(normalize : Path.normalization)
    ~(pos : Position.t)
    ~(code : string)
    ~(current_file : Path.t)
    ~(project_root : Path.t option)
    (mod_res : Preprocessor.ModRes.t option)
    : string list
  =
  let regex = Str.regexp {|#[ \t]*\(include\|import\)[ \t]*"|} in
  (* n.b.: We may not use [List.nth_exn] because if the user happens to trigger
     a completion at the last line while "Editor: Render Final Newline" is
     enabled, it will crash the language server. *)
  let current_line =
    Option.value ~default:"" @@ List.nth (String.split_lines code) pos.line
  in
  if Str.string_match regex current_line 0
  then
    Option.value_map project_root ~default:[] ~f:(fun project_root ->
        Files.list_directory ~normalize project_root
        |> List.filter_map ~f:(fun file ->
               if Path.equal current_file file
               then None
               else Option.some @@ Path.make_relative (Path.dirname current_file) file)
        |> List.append (Files.list_library_files ~normalize project_root mod_res))
  else []

(** Helper to create completion items from file names. *)
let complete_files (files : string list) : CompletionItem.t list =
  List.map files ~f:(fun file ->
      CompletionItem.create
        ~label:file
        ~kind:CompletionItemKind.File
        ~sortText:(completion_context_priority File)
        ())
