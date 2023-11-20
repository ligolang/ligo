open Handler
open Lsp_helpers
module Utils = Simple_utils.Utils

let mk_completion_list (items : CompletionItem.t list)
    : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option
  =
  Option.some @@ `CompletionList (CompletionList.create ~isIncomplete:false ~items ())


let on_req_completion (pos : Position.t) (path : Path.t)
    : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option
    Handler.t
  =
  with_cached_doc ~default:None path
  @@ fun { definitions; code; syntax } ->
  let keyword_completions = Completion_lib.Keywords.get_keyword_completions syntax in
  let project_root = Project_root.get_project_root path in
  let@ mod_res = ask_mod_res in
  let files =
    Completion_lib.Files.get_files_for_completions
      ~current_file:path
      ~project_root
      !mod_res
  in
  let file_completions = Completion_lib.Files.complete_files pos code files in
  let completions_without_cst = file_completions @ keyword_completions in
  (* Even if parsing fail for whatever reason (e.g. preprocessor error),
    we can at least show files and keywords to the user. *)
  let completions_so_far = mk_completion_list completions_without_cst in
  with_cst path ~default:completions_so_far
  @@ fun cst ->
  let input : Completion_lib.Common.input_d =
    Completion_lib.Common.mk_input_d ~cst ~path ~syntax ~definitions ~pos
  in
  let field_completions = Completion_lib.Fields.get_fields_completions input in
  let all_completions =
    (* If we are completing a record or module field, there is no need to also
       suggest scopes or keywords. *)
    if List.is_empty field_completions
    then (
      let scopes = Ligo_interface.get_scopes ~project_root ~definitions ~code path in
      Completion_lib.Scope.get_scope_completions input scopes @ completions_without_cst)
    else field_completions
  in
  return @@ mk_completion_list all_completions
