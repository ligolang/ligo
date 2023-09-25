(* This module contains wrappers for various LIGO functions:
   get-scope that collects information about definitions in a file *)
module Get_scope = Get_scope
open Get_scope

type nonrec defs_and_diagnostics = defs_and_diagnostics
type scopes = Scopes.scopes

(** To support dirty files, we store some data about files in memory *)
type file_data =
  { syntax : Syntax_types.t
  ; code : string
  ; definitions : Def.t list
  }

let lsp_raw_options : Path.t -> Compiler_options.Raw_options.t =
 fun path ->
  (* packages - project_root [later] *)
  let file = Path.to_string path in
  (* #include - Pass lib or dirs *)
  let dir_name = Filename.dirname file in
  (* FIXME [#1657]: Once we have a project system, set the correct [project_root]. *)
  let project_root = Some dir_name in
  Compiler_options.Raw_options.make
    ~with_types:true
    ~libraries:[ dir_name ]
    ~project_root
    ()


let get_defs_and_diagnostics
    : ?json_download:bool -> code:string -> Path.t -> defs_and_diagnostics Lwt.t
  =
 fun ?json_download ~code path ->
  let options = lsp_raw_options path in
  get_defs_and_diagnostics
    ?json_download
    options
    (Raw_input_lsp { file = Path.to_string path; code })


let get_scopes : definitions:Def.t list -> code:string -> Path.t -> Scopes.scopes =
 fun ~definitions ~code path ->
  let options = lsp_raw_options path in
  get_scopes options (Raw_input_lsp { file = Path.to_string path; code }) definitions
