(* This module contains wrappers for various LIGO functions:
   get-scope that collects information about definitions in a file *)
module Get_scope = Get_scope
open Get_scope
open Lsp.Types

type nonrec defs_and_diagnostics = defs_and_diagnostics
type scopes = Scopes.scopes

(** To support dirty files, we store some data about files in memory *)
type file_data =
  { syntax : Syntax_types.t
  ; code : string
  ; definitions : Def.t list
  }

let lsp_raw_options : project_root:Path.t option -> Compiler_options.Raw_options.t =
 fun ~project_root ->
  Compiler_options.Raw_options.make
    ~with_types:true
    ~project_root:(Option.map ~f:Path.to_string project_root)
    ()


let get_defs_and_diagnostics
    :  project_root:Path.t option -> code:string
    -> logger:(type_:MessageType.t -> string -> unit Lwt.t) -> Path.t
    -> defs_and_diagnostics Lwt.t
  =
 fun ~project_root ~code ~logger path ->
  let options = lsp_raw_options ~project_root in
  get_defs_and_diagnostics
    ~logger
    options
    (Raw_input_lsp { file = Path.to_string path; code })


let get_scopes
    :  project_root:Path.t option -> definitions:Def.t list -> code:string -> Path.t
    -> Scopes.scopes
  =
 fun ~project_root ~definitions ~code path ->
  let options = lsp_raw_options ~project_root in
  get_scopes options (Raw_input_lsp { file = Path.to_string path; code }) definitions
