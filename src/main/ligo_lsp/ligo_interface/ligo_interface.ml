open Scopes.Types

type get_scope_api_result =
  Main_errors.all list * Main_warnings.all list * (Scopes.def list * Scopes.scopes) option

type get_scope_info =
  { errors : Main_errors.all list
  ; warnings : Main_warnings.all list
  ; has_info : bool
  ; definitions : Scopes.def list
  ; scopes : Scopes.scopes option
  }

(** To support dirty files, we store some data about files in memory *)
type file_data =
  { syntax : Syntax_types.t
  ; code : string
  ; get_scope_info : get_scope_info
  }

module type LIGO_API = sig
  module Info : sig
    val get_scope_trace
      :  Compiler_options.Raw_options.t
      -> BuildSystem.Source_input.code_input
      -> unit
      -> get_scope_api_result
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

  type nonrec get_scope_info = get_scope_info

  let get_scope : DocumentUri.t -> string -> get_scope_api_result =
   fun uri source ->
    (* packages - project_root [later] *)
    let file = DocumentUri.to_path uri in
    (* #include - Pass lib or dirs *)
    let dir_name = Filename.dirname file in
    let compiler_options =
      Compiler_options.Raw_options.make ~with_types:true ~libraries:[ dir_name ] ()
    in
    Ligo_api.Info.get_scope_trace
      compiler_options
      (Raw_input_lsp { file ; code = source })
      ()


  let formatting : DocumentUri.t -> (string * string, string * string) result =
   fun uri ->
    let compiler_options = Compiler_options.Raw_options.make () in
    let file_path = DocumentUri.to_path uri in
    let display_format = Simple_utils.Display.human_readable in
    Ligo_api.Print.pretty_print compiler_options file_path display_format ()
end

let unfold_get_scope ((errors, warnings, plain_data_opt) : get_scope_api_result)
    : get_scope_info
  =
  let extract_defs : def list -> def list =
    let rec from_module (m : mdef) =
      match m.mod_case with
      | Alias _ -> []
      | Def defs -> from_defs defs
    and from_defs defs =
      let current_module_defs : mdef list =
        List.filter_map
          ~f:(function
            | Module m -> Some m
            | Variable _ | Type _ -> None)
          defs
      in
      defs @ List.concat_map ~f:from_module current_module_defs
    in
    from_defs
  in
  let has_info, definitions, scopes =
    match plain_data_opt with
    | None -> false, [], None
    | Some (plain_decls, scopes) -> true, extract_defs plain_decls, Some scopes
  in
  { errors; warnings; has_info; definitions; scopes }
