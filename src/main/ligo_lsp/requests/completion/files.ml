open Common
open Lsp_helpers

let get_files_for_completions
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
    Option.value_map
      ~default:[]
      ~f:(fun project_root ->
        Files.list_directory project_root
        |> List.filter_map ~f:(fun file ->
               if Path.equal current_file file
               then None
               else Option.some @@ Path.make_relative (Path.dirname current_file) file)
        |> List.append (Files.list_library_files project_root mod_res))
      project_root
  else []


let complete_files (files : string list) : CompletionItem.t list =
  List.map files ~f:(fun file ->
      CompletionItem.create
        ~label:file
        ~kind:CompletionItemKind.File
        ~sortText:(completion_context_priority File)
        ())
