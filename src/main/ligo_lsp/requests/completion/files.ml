(* TODO #2091*)
open Common
open Lsp_helpers

let complete_files (pos : Position.t) (code : string) (files : string list)
    : CompletionItem.t list
  =
  let regex = Str.regexp {|^#[ \t]*\(include\|import\)[ \t]*\"|} in
  (* n.b.: We may not use [List.nth_exn] because if the user happens to trigger
     a completion at the last line while "Editor: Render Final Newline" is
     enabled, it will crash the language server. *)
  let current_line =
    Option.value ~default:"" @@ List.nth (String.split_lines code) pos.line
  in
  if Str.string_match regex current_line 0
  then
    List.map files ~f:(fun file ->
        CompletionItem.create
          ~label:file
          ~kind:CompletionItemKind.File
          ~sortText:(completion_context_priority File)
          ())
  else []
