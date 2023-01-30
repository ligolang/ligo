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

(* 
let all_variables (_, _, defs_scopes_opt : get_scope_info ) : Scopes.Types.vdef list =
  let current_module_defs = 
    begin match defs_scopes_opt with
    | Some (defs, _) -> List.filterMap () defs
    | None -> []
    end
  in current_module_defs @ [] *)
