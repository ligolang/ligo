(* This module contains wrappers for various LIGO functions:
   get-scope that collects information about definitions in a file *)
module Get_scope = Get_scope
open Get_scope

type nonrec defs_and_diagnostics = defs_and_diagnostics
type scopes = Scopes.scopes
type definitions = Scopes.definitions

(** To support dirty files, we store some data about files in memory *)
type file_data =
  { syntax : Syntax_types.t
  ; code : string
  ; definitions : Def.definitions option
  ; scopes : scopes option
  }

let lsp_raw_options : project_root:Path.t option -> Compiler_options.Raw_options.t =
 fun ~project_root ->
  Compiler_options.Raw_options.make
    ~with_types:true
    ~project_root:(Option.map ~f:Path.to_string project_root)
    ()


let get_defs : project_root:Path.t option -> code:string -> Path.t -> definitions Lwt.t =
 fun ~project_root ~code path ->
  let options = lsp_raw_options ~project_root in
  get_defs options (Raw_input_lsp { file = Path.to_string path; code })


let get_defs_and_diagnostics
    :  project_root:Path.t option -> code:string
    -> logger:(type_:Lsp.Types.MessageType.t -> string -> unit Lwt.t) -> Path.t
    -> defs_and_diagnostics Lwt.t
  =
 fun ~project_root ~code ~logger path ->
  let options = lsp_raw_options ~project_root in
  get_defs_and_diagnostics
    ~logger
    options
    (Raw_input_lsp { file = Path.to_string path; code })


let get_scope : project_root:Path.t option -> code:string -> Path.t -> scopes =
 fun ~project_root ~code path ->
  let options = lsp_raw_options ~project_root in
  get_scopes options (Raw_input_lsp { file = Path.to_string path; code })
