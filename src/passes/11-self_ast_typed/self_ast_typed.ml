module Errors = Errors
module Helpers = Helpers

let all_program ~raise init = Make_entry_point.make_main_module ~raise init

let all_contract ~raise entrypoints module_path (prg : Ast_typed.program) =
  let module_ = Helpers.get_module module_path prg in
  let module_ =
    match entrypoints with
    | [] -> module_
    | _ ->
      Helpers.strip_entry_annotations module_
      |> Helpers.annotate_with_entry ~raise entrypoints
  in
  let main_name, module_ = Make_entry_point.program ~raise module_ in
  let prg, () = Helpers.update_module module_path (fun _ -> module_, ()) prg in
  let prg, main_name, contract_type =
    Helpers.fetch_contract_type ~raise main_name module_path prg
  in
  (main_name, contract_type), prg


let all_view ~(raise : (_,_) Simple_utils.Trace.raise) main_name module_path contract_type prg =
  let module_ = Helpers.get_module module_path prg in
  let () =
    match
      Helpers.get_shadowed_decl module_ (fun ({ view; _ } : Ast_typed.ValueAttr.t) ->
          view)
    with
    | Some loc -> raise.error (Errors.annotated_declaration_shadowed loc)
    | None -> ()
  in
  let f decl =
    match Ast_typed.Helpers.fetch_view_type decl with
    | None -> ()
    | Some (view_type, view_binder) ->
      View_passes.check_view_type
        ~raise
        ~err_data:(main_name, view_binder)
        contract_type
        view_type
  in
  let () = List.iter ~f module_ in
  let prg, () = Helpers.update_module module_path (fun _ -> module_, ()) prg in
  prg
