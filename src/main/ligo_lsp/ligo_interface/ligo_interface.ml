type get_scope_info =
  Main_errors.all list * Main_warnings.all list * (Scopes.def list * Scopes.scopes) option

module type LIGO_API = sig
  module Info : sig
    val get_scope_trace
      :  Compiler_options.Raw_options.t
      -> BuildSystem.Source_input.code_input
      -> unit
      -> get_scope_info
  end

  module Print : sig
    val pretty_print
      :  Compiler_options.Raw_options.t
      -> string
      -> Simple_utils.Display.ex_display_format
      -> unit
      -> (string * string, string * string) result
  end
end

module Make (Ligo_api : LIGO_API) = struct
  open Lsp.Types
  open BuildSystem.Source_input

  type nonrec get_scope_info = get_scope_info

  let get_scope : DocumentUri.t -> string -> get_scope_info =
   fun uri source ->
    (* packages - project_root [later] *)
    let file_path = DocumentUri.to_path uri in
    (* #include - Pass lib or dirs *)
    let dir_name = Filename.dirname file_path in
    let compiler_options =
      Compiler_options.Raw_options.make ~with_types:true ~libraries:[ dir_name ] ()
    in
    Ligo_api.Info.get_scope_trace
      compiler_options
      (Raw { id = file_path; code = source })
      ()


  let formatting : DocumentUri.t -> (string * string, string * string) result =
   fun uri ->
    let compiler_options = Compiler_options.Raw_options.make () in
    let file_path = DocumentUri.to_path uri in
    let display_format = Simple_utils.Display.human_readable in
    Ligo_api.Print.pretty_print compiler_options file_path display_format ()
end

let (extract_variables, extract_types, extract_modules) :
      (Scopes.Types.def list -> Scopes.Types.vdef list)
      * (Scopes.Types.def list -> Scopes.Types.tdef list)
      * (Scopes.Types.def list -> Scopes.Types.mdef list)
  =
  let extract_with f =
    let rec from_module (m : Scopes.Types.mdef) =
      match m.mod_case with
      | Alias _ -> []
      | Def defs -> from_defs defs
    and from_defs defs =
      let current_extracted_defs = List.filter_map ~f defs
      and current_module_defs : Scopes.Types.mdef list =
        List.filter_map
          ~f:(function
            | Scopes.Types.Module m -> Some m
            | _ -> None)
          defs
      in
      current_extracted_defs @ List.concat_map ~f:from_module current_module_defs
    in
    from_defs
  in
  ( extract_with (function
        | Scopes.Types.Variable v -> Some v
        | _ -> None)
  , extract_with (function
        | Scopes.Types.Type t -> Some t
        | _ -> None)
  , extract_with (function
        | Scopes.Types.Module m -> Some m
        | _ -> None) )


let unfold_get_scope ((errs, warnings, plain_data_opt) : get_scope_info) : get_scope_info =
  let data =
    match plain_data_opt with
    | None -> None
    | Some (plain_decls, scopes) ->
      let decls =
        List.map ~f:(fun x -> Scopes.Types.Variable x) (extract_variables plain_decls)
        @ List.map ~f:(fun x -> Scopes.Types.Type x) (extract_types plain_decls)
        @ List.map ~f:(fun x -> Scopes.Types.Module x) (extract_modules plain_decls)
      in
      Some (decls, scopes)
  in
  errs, warnings, data
