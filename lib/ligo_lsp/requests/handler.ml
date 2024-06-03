open Core
open Lsp_helpers
module Path_hashtbl = Hashtbl.Make (Path)
module Ligo_fun = Simple_utils.Ligo_fun

let ( <@ ) = Ligo_fun.( <@ )
let uncurry = Ligo_fun.uncurry

(** This is a hash table that stores file data for each file path that was opened. See
    {!Ligo_interface.unprepared_file_data}. *)
module Docs_cache = struct
  include Path_hashtbl (* So we can call e.g. [Docs_cache.find] *)

  type t = Ligo_interface.unprepared_file_data Path_hashtbl.t

  let create : unit -> t = fun () -> Path_hashtbl.create ~size:32 ()
end

(** Stores the configuration pertaining to the LIGO language server. *)
type config =
  { max_number_of_problems : int
        (** The maximum number of diagnostics to be shown. Defaults to 100. *)
  ; logging_verbosity : MessageType.t
        (** The level of verbosity when logging. Defaults to Info. *)
  ; disabled_features : string list
        (** Disabled requests, i.e., they are not handled by the language server. Defaults to []. *)
  ; max_line_width : int option
        (** Override the max line width for formatted file (80 by default) *)
  ; completion_implementation :
      [ `With_scopes | `All_definitions | `Only_keywords_and_fields ]
        (** There are different ways to show a LSP completion: we can
            - [`With_scopes]: provide all definitions available in current scope, but this
              is a bit slower since it uses [Def.Hierarchy].
            - [`All_definitions]: provide all definitions in current file, including
              things that are not available in the current scope.
            - [`OnlyKeywordsAndRecordFields]: provide record/module fields if the coursor
              is located after a dot (like M.x|), otherwise provide keywords only.
              This is fast and always correct, but don't contain even the stdlib stuff. *)
  ; diagnostics_pull_mode : [ `OnDocUpdate | `OnDocumentLinkRequest | `OnSave ]
        (** when should we process the document?
            - [`OnDocUpdate]: immediatly after we've got a new version
            - [`OnDocumentLinkRequest]: when editor asks us for document links
              (vscode do this automatically after user stops typing)
            - [`OnSave]: when the document was saved *)
  ; metadata_checks_downloading : bool
        (** When running metadata checks, whether to download and check
            mentioned external resources *)
  ; metadata_checks_download_timeout_sec : float
        (** When running metadata checks, which timeout (in seconds) will be
            used for downloading from external sources. Timeout is mandatory,
            in LSP metadata downloading is secondary functionality and we
            don't want downloads to block diagnostics indefinitely. *)
  }

(** We can send diagnostics to user or just save them to list in case of testing *)
type notify_back_mockable =
  | Normal of Linol_lwt.Jsonrpc2.notify_back
      (** Contains the current URI (coming from the LSP method) that will be
          used for publishing diagnostics as well as the class that may be used
          for interacting with the LSP client. *)
  (* FIXME: collect logs for tests *)
  | Mock of Diagnostic.t list Path_hashtbl.t
      (** A mock environment that may used to collect diagnostics for tests. *)

(** Environment available in [handler] monad. *)
type env =
  { notify_back : notify_back_mockable
        (** Allows sending a notification or request back to the client. *)
  ; config : config (** The configuration provided by the client. *)
  ; docs_cache : Docs_cache.t (** The cache for each open file. *)
  ; last_project_dir : Path.t option ref (** The path to the last seen project root. *)
  ; mod_res : Preprocessor.ModRes.t option ref
        (** The mod res can be used to resolve imports of LIGO registry packages into a
            file path. *)
  ; normalize : string -> Path.t
        (** A function that's used to canonicalize a file path. See {!Path.from_absolute}
            and [ask_normalize]. We recommend using the function provided by
            [ask_normalize] for performance. *)
  ; metadata_download_options : Tzip16_storage.download_options
        (** Download options for diagnostics that check for TZIP-16-compatible storages. *)
  }

module T = struct
  (** The handler monad ['a t] allows sending messages to user
      and reading docs cache by making use of the [env]. *)
  type 'a t = Handler of (env -> 'a IO.t) [@@unboxed]

  (** Run a handler with a given environment [env]. *)
  let run (Handler t) env = t env

  let return (a : 'a) : 'a t = Handler (fun _ -> IO.return a)

  let bind t ~f =
    Handler
      IO.(
        fun env ->
          let* x = run t env in
          run (f x) env)

  let map = `Define_using_bind
end

include T (** @inline *)

include Monad.Make (T)

type 'a handler = 'a t

(** The unit type wrapped in a [handler]. The same as [return ()]. *)
let pass : unit t = return ()

(** Run an [IO.t] action in the context of a [handler]. *)
let lift_io (m : 'a IO.t) : 'a t = Handler (fun _ -> m)

(** Get the environment inside the monad computation. *)
let ask : env t = Handler IO.return

(** Get the [notify_back] inside the monad computation. *)
let ask_notify_back : notify_back_mockable t = ask >>| fun x -> x.notify_back

(** Get the [config] inside the monad computation. *)
let ask_config : config t = ask >>| fun x -> x.config

(** Get the [docs_cache] inside the monad computation. *)
let ask_docs_cache : Docs_cache.t t = ask >>| fun x -> x.docs_cache

(** Get the [last_project_dir] inside the monad computation. *)
let ask_last_project_dir : Path.t option ref t = ask >>| fun x -> x.last_project_dir

(** Get the [mod_res] inside the monad computation. *)
let ask_mod_res : Preprocessor.ModRes.t option ref t = ask >>| fun x -> x.mod_res

(** Get the [normalize] inside the monad computation. *)
let ask_normalize : (string -> Path.t) t = ask >>| fun x -> x.normalize

(** Get the [metadata_download_options] inside the monad computation. *)
let ask_metadata_download_options : Tzip16_storage.download_options t =
  ask >>| fun x -> x.metadata_download_options

(** Add or overwrite the given path to contain the provided file data. *)
let set_docs_cache (path : Path.t) (data : Ligo_interface.unprepared_file_data) : unit t =
  let open Let_syntax in
  let%map docs_cache = ask_docs_cache in
  Hashtbl.set docs_cache ~key:path ~data

(** Sequencing handlers *)

(** Iterate over a list, consuming its elements. *)
let iter (xs : 'a list) ~f:(h : 'a -> unit t) : unit t =
  let rec go = function
    | [] -> pass
    | x :: xs -> h x >>= fun () -> go xs
  in
  go xs

(** Fold over a list while performing an action on them. *)
let rec fold_left ~(init : 'acc) ~(f : 'acc -> 'elt -> 'acc t) : 'elt list -> 'acc t
  = function
  | [] -> return init
  | x :: xs -> f init x >>= fun init -> fold_left ~init ~f xs

(** Map over a list while performing an action that may create more elements. Concat the
    results. *)
let concat_map ~(f : 'a -> 'b list t) : 'a list -> 'b list t =
  let rec go acc = function
    | [] -> return (List.rev acc)
    | x :: xs -> f x >>= fun ys -> go (List.rev_append ys acc) xs
  in
  go []

(** Conditional computations *)

(** Consume an action if the given boolean is [true] or [pass] otherwise.
    Note: OCaml is strict, so [when_ b m] may still perform "leading" non-monadic actions
    in [m] eagerly, even if [b = false]. *)
let when_ (b : bool) (m : unit t) : unit t = if b then m else pass

(** Map over [Some] while performing an action. *)
let when_some (m_opt : 'a option) (f : 'a -> 'b t) : 'b option t =
  match m_opt with
  | Some m -> f m >>| Option.some
  | None -> return None

(** Iterate over [Some] performing an [unit]-producing action or [pass] otherwise. *)
let when_some_ (m_opt : 'a option) (f : 'a -> unit t) : unit t =
  match m_opt with
  | Some m -> f m
  | None -> pass

(** Bind over an action if it produced [Some]. *)
let when_some' (m_opt : 'a option) (f : 'a -> 'b option t) : 'b option t =
  match m_opt with
  | Some m -> f m
  | None -> return None

(** Sends a message to the client with the provided logging level. In VSCode, for
    instance, it will appear in the Output > LIGO Language Server window. *)
let send_log_msg ~(type_ : MessageType.t) (s : string) : unit t =
  let open Let_syntax in
  match%bind ask_notify_back with
  | Normal nb ->
    let%bind { logging_verbosity; _ } = ask_config in
    if Stdlib.(type_ <= logging_verbosity)
    then lift_io @@ nb#send_log_msg ~type_ s
    else pass
  | Mock _ -> pass

(** Send diagnostics (errors, warnings) to the LSP client. *)
let send_diagnostic (uri : DocumentUri.t) (s : Diagnostic.t list) : unit t =
  let open Let_syntax in
  match%bind ask_notify_back with
  | Normal nb ->
    let old_uri = nb#get_uri in
    let () = nb#set_uri uri in
    let%bind () = lift_io (nb#send_diagnostic s) in
    (match old_uri with
    (* FIXME: linol doesn't allow setting [None], change it when it'll be fixed there *)
    | None -> ()
    | Some old_uri -> nb#set_uri old_uri);
    pass
  | Mock diagnostics ->
    let%map normalize = ask_normalize in
    Hashtbl.update
      diagnostics
      (DocumentUri.to_path ~normalize uri)
      ~f:(Option.value_map ~default:s ~f:(( @ ) s))

(** Message will appear in log in case [logging_verbosity = MessageType.Log]. *)
let send_debug_msg : string -> unit t = send_log_msg ~type_:MessageType.Log

(** Displays a pop-up message. *)
let send_message ?(type_ : MessageType.t = Info) (message : string) : unit t =
  let open Let_syntax in
  let%bind nb = ask_notify_back in
  match nb with
  | Normal nb ->
    lift_io
      (nb#send_notification @@ ShowMessage (ShowMessageParams.create ~message ~type_))
  | Mock _ -> pass

(** Helper data type to unlift a [t] action into an [IO.t] *)
type unlift_io = { unlift_io : 'a. 'a t -> 'a IO.t }

(** Allows unlifting some [t] action to be run in [IO.t] in the context of a
    [t]. *)
let with_run_in_io : (unlift_io -> 'b IO.t) -> 'b t =
 fun inner -> Handler (fun env -> inner { unlift_io = (fun t -> run t env) })

(** Sends a LSP request from the server to the client. *)
let send_request
    (request : 'a Server_request.t)
    (handler : ('a, Jsonrpc.Response.Error.t) result -> unit t)
    : unit t
  =
  let open Let_syntax in
  match%bind ask_notify_back with
  | Normal nb ->
    let%bind _ =
      with_run_in_io @@ fun { unlift_io } -> nb#send_request request (unlift_io <@ handler)
    in
    pass
  | Mock _ -> pass

(** List [send_message], but also sends a list of options that the user can click on.
    [handler] is a callback that will be called once the user has pressed some button. *)
let send_message_with_buttons
    ~(message : string)
    ~(options : string list)
    ~(type_ : MessageType.t)
    ~(handler : (MessageActionItem.t option, Jsonrpc.Response.Error.t) result -> unit t)
    : unit t
  =
  let actions = List.map options ~f:(fun title -> MessageActionItem.create ~title) in
  let smr =
    Server_request.ShowMessageRequest
      (ShowMessageRequestParams.create ~actions ~message ~type_ ())
  in
  send_request smr handler

(** Attempts to get the LIGO syntax of the provided path, or fails otherwise. *)
let get_syntax_exn : Path.t -> Syntax_types.t t =
 fun path ->
  match Path.get_syntax path with
  | None ->
    lift_io @@ IO.failwith @@ "Expected file with LIGO code, got: " ^ Path.to_string path
  | Some s -> return s

(** Use document info from cache. In case it was not found, returns the [default] value.
    This can happen only if we obtained a request for document that was not opened, so
    it's fine to return things like [None] in this case. In case the document was not
    processed yet then it will process the document to fill the caches and send
    diagnostics. *)
let with_cached_doc ~(default : 'a) (path : Path.t) (f : Ligo_interface.file_data -> 'a t)
    : 'a t
  =
  let open Let_syntax in
  let%bind docs = ask_docs_cache in
  match Hashtbl.find docs path with
  | None -> return default
  | Some
      { code
      ; syntax
      ; document_version
      ; definitions
      ; hierarchy
      ; potential_tzip16_storages
      ; parse_error_ranges
      ; lambda_types
      } ->
    (match definitions, hierarchy, potential_tzip16_storages with
    | Some definitions, Some hierarchy, Some potential_tzip16_storages ->
      let%bind () = send_debug_msg "Found definitions, hierarchy and TZIP-16 storages" in
      f
        { code
        ; syntax
        ; document_version
        ; definitions
        ; hierarchy
        ; potential_tzip16_storages
        ; parse_error_ranges
        ; lambda_types
        }
    | _ ->
      let%bind () = send_debug_msg "Recalculating caches" in
      let%bind last_project_dir = ask_last_project_dir in
      let project_root = !last_project_dir in
      let%bind syntax = get_syntax_exn path in
      let%bind tzip16_download_options = ask_metadata_download_options in
      let%bind ({ definitions; potential_tzip16_storages; _ } as defs_and_diagnostics) =
        with_run_in_io
        @@ fun { unlift_io } ->
        Ligo_interface.get_defs_and_diagnostics
          ~project_root
          ~code
          ~logger:(fun ~type_ msg -> unlift_io @@ send_log_msg ~type_ msg)
          ~tzip16_download_options
          path
      in
      let%bind normalize = ask_normalize in
      let hierarchy = lazy (Def.Hierarchy.create ~normalize definitions) in
      let parse_error_ranges =
        List.fold defs_and_diagnostics.errors ~init:[] ~f:(fun acc -> function
          | `Parser_tracer (`Parsing { value = _; region }) ->
            Range.of_region region :: acc
          | _ -> acc)
      in
      let%bind () =
        set_docs_cache
          path
          { code
          ; syntax
          ; document_version
          ; definitions = Some definitions
          ; hierarchy = Some hierarchy
          ; potential_tzip16_storages = Some potential_tzip16_storages
          ; parse_error_ranges
          ; lambda_types = defs_and_diagnostics.lambda_types
          }
      in
      let%bind max_number_of_problems =
        ask_config >>| fun x -> x.max_number_of_problems
      in
      let diags_by_file =
        let simple_diags =
          Diagnostics.get_diagnostics ~normalize path defs_and_diagnostics
        in
        Diagnostics.partition_simple_diagnostics
          path
          (Some max_number_of_problems)
          simple_diags
      in
      (* Corner case: clear diagnostics for this file in case there are none. *)
      let%bind () =
        let uri = DocumentUri.of_path path in
        if List.Assoc.mem diags_by_file ~equal:DocumentUri.equal uri
        then pass
        else send_diagnostic uri []
      in
      let%bind () = iter diags_by_file ~f:(uncurry send_diagnostic) in
      f
        { code
        ; syntax
        ; document_version
        ; definitions
        ; hierarchy
        ; potential_tzip16_storages
        ; parse_error_ranges
        ; lambda_types = defs_and_diagnostics.lambda_types
        })

(** Calculates definitions and scopes for the given file path, saving them to the cache
    and sending diagnostics to the client. If the document was already processed then this
    function will do nothing. *)
let process_doc (path : Path.t) : unit t =
  with_cached_doc path ~default:() (fun _ -> pass)

(** The same as [with_cached_doc], but the provided function doesn't need to produce a
    [t]. This is just a convenience to avoid calling [return]. *)
let with_cached_doc_pure
    ~(default : 'a)
    (path : Path.t)
    (f : Ligo_interface.file_data -> 'a)
    : 'a t
  =
  let f' = return <@ f in
  with_cached_doc path ~default f'

(** Like [with_cached_doc], but only provide the source code. *)
let with_code (path : Path.t) ~(default : 'a) (f : string -> 'a t) : 'a t =
  let open Let_syntax in
  let%bind docs = ask_docs_cache in
  match Hashtbl.find docs path with
  | Some file_data -> f file_data.code
  | None -> return default

(** Like [with_cached_doc], but parses a CST from code. If [strict] is passed, then the
    error recovery for parsing will be disabled, and the [default] value will be returned
  in case of any parse error.
    [on_error] allows to display a pop-up instead of just adding a debug message in case
  of an error. This is helpful for displaying to the user why their request failed. We
  should not display a pop-up when an automated request (e.g. document link) failed, but
  should when e.g. formatting failed because of a syntax error. *)
let with_cst
    ~(default : 'a)
    ?(strict = false)
    ?(on_error : string -> unit handler =
      fun err -> send_debug_msg @@ "Unable to get CST: " ^ err)
    (path : Path.t)
    (f : Dialect_cst.t -> 'a t)
    : 'a t
  =
  with_code ~default path
  @@ fun code ->
  let open Let_syntax in
  let%bind syntax = get_syntax_exn path in
  match
    let buffer = Buffer.create (String.length code) in
    Buffer.add_string buffer code;
    Ligo_api.Dialect_cst.get_cst
      ?project_root:(Option.map ~f:Path.to_string @@ Project_root.get_project_root path)
      ~preprocess_define:(* TODO: extract from project root ? *) []
      ~strict
      ~file:(Path.to_string path)
      syntax
      buffer
  with
  | Error err ->
    let%bind () = on_error err in
    return default
  | Ok cst -> f cst

(** Sets the project root to the provided path, by updating [last_project_dir] and
    [mod_res] accordingly.*)
let update_project_root : Path.t option -> unit t =
 fun path ->
  let open Let_syntax in
  let%bind last_project_dir = ask_last_project_dir in
  let%bind mod_res = ask_mod_res in
  return
  @@
  match path with
  | None ->
    last_project_dir := None;
    mod_res := None
  | Some path ->
    last_project_dir := Some path;
    mod_res := Preprocessor.ModRes.make (Path.to_string path)
