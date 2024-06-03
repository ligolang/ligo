(* This module contains wrappers for various LIGO functions:
   get-scope that collects information about definitions in a file *)

open Core
module Location = Simple_utils.Location
module Get_scope = Get_scope
open Get_scope

type nonrec defs_and_diagnostics = defs_and_diagnostics
type definitions = Scopes.definitions

(** Version of the document with respect to all of its edits, see
    [VersionedTextDocumentIdentifier] in the LSP specification. *)
type document_version = int [@@deriving yojson]

(** A list containing all storage names that were identified as being potentially
    TZIP-16-compatible. *)
type potential_tzip16_storages = Ast_typed.expression_variable list

(** Stores some useful information and caches for a given document. *)
type ('defs, 'hierarchy, 'pot_tzip16) file_data_case =
  { syntax : Syntax_types.t (** The LIGO syntax of this file. *)
  ; code : string (** The source code of this file. *)
  ; document_version : document_version option
        (* Invariant: always set for an opened document *)
        (** Version of the document with respect to all of its edits, see
            [VersionedTextDocumentIdentifier] in the LSP specification. *)
  ; parse_error_ranges : Range.t list
        (** A list of ranges where parse errors occurred.
            It's useful when we want to determine whether the declaration
            is error-recovered.

            For example:
            {[
              let f (x y z) = ()
            ]}
            would be recovered into
            {[
              let f (x) y z = ()
            ]}
            If the part from [let] up to [=] contains parse error ranges
            then it's recovered. *)
  ; lambda_types : Ast_typed.ty_expr Location.Map.t
        (** A map from lambda location to its type.

            Suppose we have the next declaration:
            {[
              let f = fun x y -> x + y
            ]}
            In this case the lambda location starts from the [fun] word
            and stops at the end of the expression (after [x + y] in this case).
            The type is [int]. *)
  ; definitions : 'defs (** Cached [Def.t]s obtained for this file. *)
  ; hierarchy : 'hierarchy (** Cached [Def.hierarchy] for this file. *)
  ; potential_tzip16_storages : 'pot_tzip16
        (** Cached storage names that were identified as being potentially
            TZIP-16-compatible. *)
  }

(** [None] in definitions / scopes means that file was not processed.
    See [on_doc] and [process_doc] *)
type unprepared_file_data =
  ( Def.definitions option
  , Def.Hierarchy.t lazy_t option
  , potential_tzip16_storages option )
  file_data_case

(** Create a file data with cleared caches. *)
let empty
    ~(syntax : Syntax_types.t)
    ~(code : string)
    ~(document_version : document_version option)
    : unprepared_file_data
  =
  { syntax
  ; code
  ; document_version
  ; parse_error_ranges = []
  ; lambda_types = Location.Map.empty
  ; definitions = None
  ; hierarchy = None
  ; potential_tzip16_storages = None
  }

(** Like [unprepared_file_data], but with all [option]s removed (caches are filled). *)
type file_data =
  (Def.definitions, Def.Hierarchy.t lazy_t, potential_tzip16_storages) file_data_case

(** The compiler options that make a good default for the language server. See
    {!Project_root} on how to obtain the [project_root]. *)
let lsp_raw_options : project_root:Path.t option -> Compiler_options.Raw_options.t =
 fun ~project_root ->
  Compiler_options.Raw_options.make
    ~with_types:true
    ~no_metadata_check:true (* We handle TZIP-16 metadata checks separately *)
    ~project_root:(Option.map ~f:Path.to_string project_root)
    ~typer_error_recovery:true
    ()

(** Calculates the [Def.t]s and diagnostics for a given file. See {!Project_root} on how
    to obtain the [project_root]. [code] is the source code of this file. [logger] is a
    function that will log messages (see [Requests.Handler.send_log_msg] for a good choice
    of logger). [tzip16_download_options] is used to control whether to display
    diagnostics that require downloads from TZIP-16-compatible storages. *)
let get_defs_and_diagnostics
    :  project_root:Path.t option -> code:string
    -> logger:(type_:Lsp.Types.MessageType.t -> string -> unit Lwt.t)
    -> tzip16_download_options:Tzip16_storage.download_options -> Path.t
    -> defs_and_diagnostics Lwt.t
  =
 fun ~project_root ~code ~logger ~tzip16_download_options path ->
  let options = lsp_raw_options ~project_root in
  get_defs_and_diagnostics ~logger ~tzip16_download_options options path code
