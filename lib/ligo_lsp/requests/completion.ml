open Core
open Handler
open Lsp_helpers

(** A helper to create a completion list from the given completion items. Always returns
    [Some]. *)
let mk_completion_list (items : CompletionItem.t list)
    : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option
  =
  Option.some @@ `CompletionList (CompletionList.create ~isIncomplete:false ~items ())

(** Runs the handler for completion. This is usually called when the user starts to type
    something, and specially after a [.] or [@]. This request may also be triggered
    manually by the user. *)
let on_req_completion (pos : Position.t) (path : Path.t)
    : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option
    Handler.t
  =
  let open Handler.Let_syntax in
  let%bind completion_implementation =
    ask_config >>| fun c -> c.completion_implementation
  in
  let%bind () =
    send_debug_msg
    @@ sprintf
         "On completion request: %s, %s, mode: %s"
         (Path.to_string path)
         (Position.to_string pos)
         (match completion_implementation with
         | `All_definitions -> "All_definitions"
         | `With_scopes -> "With_scopes"
         | `Only_keywords_and_fields -> "Only_keywords_and_fields")
  in
  with_cached_doc ~default:None path
  @@ fun { definitions
         ; code
         ; syntax
         ; hierarchy
         ; document_version = _
         ; parse_error_ranges = _
         ; potential_tzip16_storages = _
         ; lambda_types = _
         } ->
  let keyword_completions = Completion_lib.Keywords.get_keyword_completions syntax in
  let project_root = Project_root.get_project_root path in
  let%bind mod_res = ask_mod_res in
  let%bind normalize = ask_normalize in
  let files =
    Completion_lib.Files.get_files_for_completions
      ~normalize
      ~pos
      ~code
      ~current_file:path
      ~project_root
      !mod_res
  in
  let file_completions = Completion_lib.Files.complete_files files in
  let completions_without_cst = file_completions @ keyword_completions in
  (* Even if parsing fail for whatever reason (e.g. preprocessor error),
    we can at least show files and keywords to the user. *)
  let completions_so_far = mk_completion_list completions_without_cst in
  with_cst path ~default:completions_so_far
  @@ fun cst ->
  let input : Completion_lib.Common.input_d =
    Completion_lib.Common.mk_input_d ~cst ~path ~syntax ~definitions ~pos
  in
  let field_completions = Completion_lib.Fields.get_fields_completions ~normalize input in
  let%bind all_completions =
    (* If we are completing a record or module field, there is no need to also
       suggest scopes or keywords. *)
    if not (List.is_empty field_completions)
    then return field_completions
    else (
      match completion_implementation with
      | `Only_keywords_and_fields -> return completions_without_cst
      | `All_definitions ->
        return
        @@ Completion_lib.Common.nub_sort_items
             (Completion_lib.Common.defs_to_completion_items
                ~normalize
                Scope
                path
                syntax
                (Scopes.Types.flatten_defs definitions))
        @ completions_without_cst
      | `With_scopes ->
        return
        @@ Completion_lib.Scope.get_scope_completions ~normalize input (force hierarchy)
        @ completions_without_cst)
  in
  return @@ mk_completion_list all_completions
