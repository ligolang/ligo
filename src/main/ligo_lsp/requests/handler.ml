open Lsp_helpers
module Path_hashtbl = Hashtbl.Make (Path)

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
            - [`With_scopes]: provide all definitions available in current scope, but this is slow
              since it uses [Ligo_interface.get_scopes]
            - [`All_definitions]: provide all definitions in current file, including
              things that are not available in the current scope
            - [`OnlyKeywordsAndRecordFields]: provide record/module fields if the coursor
              is located after a dot (like M.x|), otherwise provide keywords only.
              This is fast and always correct, but don't contain even the stdlib stuff. *)
  ; diagnostics_pull_mode : [ `OnDocUpdate | `OnDocumentLinkRequest | `OnSave ]
        (** when should we process the document?
            - [`OnDocUpdate]: immediatly after we've got a new version
            - [`OnDocumentLinkRequest]: when editor asks us for document links
              (vscode do this automatically after user stops typing)
            - [`OnSave]: when the document was saved *)
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

(** Environment available in Handler monad *)
type handler_env =
  { notify_back : notify_back_mockable
  ; config : config
  ; docs_cache : Docs_cache.t
  ; last_project_dir : Path.t option ref
  ; mod_res : Preprocessor.ModRes.t option ref
  }

(** Handler monad : allows sending messages to user and reading docs cache *)
type 'a handler = Handler of (handler_env -> 'a IO.t) [@@unboxed]

module Handler = struct
  type 'a t = 'a handler
end

let return (a : 'a) : 'a Handler.t = Handler (fun _ -> IO.return a)
let pass : unit Handler.t = return ()

type 'a t = 'a Handler.t

let un_handler (Handler a : 'a Handler.t) : handler_env -> 'a IO.t = a
let run_handler (env : handler_env) (r : 'a Handler.t) : 'a IO.t = un_handler r env

let bind (Handler d : 'a Handler.t) (f : 'a -> 'b Handler.t) : 'b Handler.t =
  Handler
    IO.(
      fun env ->
        let* p = d env in
        un_handler (f p) env)


let ( let@ ) = bind

(** Handler monad is a functor *)
let fmap (f : 'a -> 'b) (x : 'a Handler.t) : 'b Handler.t =
  let@ x' = x in
  return @@ f x'


let void (x : 'a Handler.t) : unit Handler.t = fmap (fun _ -> ()) x

(** We can run things from [Linol_lwt.IO] monad here *)
let lift_IO (m : 'a IO.t) : 'a Handler.t = Handler (fun _ -> m)

(** Get env inside monad computation *)
let ask : handler_env Handler.t = Handler IO.return

let ask_notify_back : notify_back_mockable Handler.t = fmap (fun x -> x.notify_back) ask
let ask_config : config Handler.t = fmap (fun x -> x.config) ask
let ask_docs_cache : Docs_cache.t Handler.t = fmap (fun x -> x.docs_cache) ask

let ask_last_project_dir : Path.t option ref Handler.t =
  fmap (fun x -> x.last_project_dir) ask


let ask_mod_res : Preprocessor.ModRes.t option ref Handler.t =
  fmap (fun x -> x.mod_res) ask


let set_docs_cache (path : Path.t) (data : Ligo_interface.unprepared_file_data)
    : unit Handler.t
  =
  let@ docs_cache = ask_docs_cache in
  return @@ Docs_cache.set docs_cache ~key:path ~data


(** Sequencing handlers *)

let iter (xs : 'a list) ~f:(h : 'a -> unit Handler.t) : unit Handler.t =
  let rec go = function
    | [] -> pass
    | x :: xs -> bind (h x) (fun () -> go xs)
  in
  go xs


let rec fold_left ~(init : 'acc) ~(f : 'acc -> 'elt -> 'acc t) : 'elt list -> 'acc t
  = function
  | [] -> return init
  | x :: xs -> bind (f init x) (fun init -> fold_left ~init ~f xs)


let concat_map ~(f : 'a -> 'b list t) : 'a list -> 'b list t =
  let rec go acc = function
    | [] -> return (List.rev acc)
    | x :: xs -> bind (f x) (fun ys -> go (List.rev_append ys acc) xs)
  in
  go []


(** Conditional computations *)

let when_ (b : bool) (m : unit Handler.t) : unit Handler.t = if b then m else pass

let when_some (m_opt : 'a option) (f : 'a -> 'b Handler.t) : 'b option Handler.t =
  match m_opt with
  | Some m -> fmap Option.some (f m)
  | None -> return None


let when_some_ (m_opt : 'a option) (f : 'a -> unit Handler.t) : unit Handler.t =
  match m_opt with
  | Some m -> f m
  | None -> pass


let when_some' (m_opt : 'a option) (f : 'a -> 'b option Handler.t) : 'b option Handler.t =
  match m_opt with
  | Some m -> f m
  | None -> return None


let when_some_m (m_opt_monadic : 'a option Handler.t) (f : 'a -> 'b Handler.t)
    : 'b option Handler.t
  =
  let@ m_opt = m_opt_monadic in
  when_some m_opt f


let when_some_m' (m_opt_monadic : 'a option Handler.t) (f : 'a -> 'b option Handler.t)
    : 'b option Handler.t
  =
  let@ m_opt = m_opt_monadic in
  when_some' m_opt f


let sequence_m (lst : 'a Handler.t list) : 'a list Handler.t =
  fmap List.rev
  @@ List.fold_left
       ~init:(return [])
       ~f:(fun acc_m elt_m ->
         let@ elt = elt_m in
         fmap (fun acc -> elt :: acc) acc_m)
       lst


let send_log_msg ~(type_ : MessageType.t) (s : string) : unit Handler.t =
  let@ nb = ask_notify_back in
  match nb with
  | Normal nb ->
    let@ { logging_verbosity; _ } = ask_config in
    if Caml.(type_ <= logging_verbosity)
    then lift_IO @@ nb#send_log_msg ~type_ s
    else pass
  | Mock _ -> pass


(** Send diagnostics (errors, warnings) to the LSP client. *)
let send_diagnostic (uri : DocumentUri.t) (s : Diagnostic.t list) : unit Handler.t =
  let@ nb = ask_notify_back in
  match nb with
  | Normal nb ->
    let old_uri = nb#get_uri in
    let () = nb#set_uri uri in
    let@ () = lift_IO (nb#send_diagnostic s) in
    (match old_uri with
    (* FIXME: linol doesn't allow setting [None], change it when it'll be fixed there *)
    | None -> ()
    | Some old_uri -> nb#set_uri old_uri);
    pass
  | Mock diagnostics ->
    return
      (Path_hashtbl.update
         diagnostics
         (DocumentUri.to_path uri)
         ~f:(Option.value_map ~default:s ~f:(( @ ) s)))


(** Message will appear in log in case [logging_verbosity = MessageType.Log] *)
let send_debug_msg : string -> unit Handler.t = send_log_msg ~type_:MessageType.Log

(** Displays a pop-up message *)
let send_message ?(type_ : MessageType.t = Info) (message : string) : unit Handler.t =
  let@ nb = ask_notify_back in
  match nb with
  | Normal nb ->
    lift_IO
      (nb#send_notification @@ ShowMessage (ShowMessageParams.create ~message ~type_))
  | Mock _ -> pass


type unlift_IO = { unlift_IO : 'a. 'a Handler.t -> 'a IO.t }

let with_run_in_IO : (unlift_IO -> 'b IO.t) -> 'b Handler.t =
 fun inner -> Handler (fun env -> inner { unlift_IO = (fun x -> run_handler env x) })


let send_request
    (request : 'a Server_request.t)
    (handler : ('a, Jsonrpc.Response.Error.t) result -> unit Handler.t)
    : unit Handler.t
  =
  let@ nb = ask_notify_back in
  match nb with
  | Normal nb ->
    fmap ignore
    @@ with_run_in_IO
    @@ fun { unlift_IO } -> nb#send_request request (unlift_IO <@ handler)
  | Mock _ -> pass


let send_message_with_buttons
    ~(message : string)
    ~(options : string list)
    ~(type_ : MessageType.t)
    ~(handler : (MessageActionItem.t option, Jsonrpc.Response.Error.t) result -> unit t)
    : unit Handler.t
  =
  let actions = List.map options ~f:(fun title -> MessageActionItem.create ~title) in
  let smr =
    Server_request.ShowMessageRequest
      (ShowMessageRequestParams.create ~actions ~message ~type_ ())
  in
  send_request smr handler


let get_syntax_exn : Path.t -> Syntax_types.t t =
 fun path ->
  match Path.get_syntax path with
  | None ->
    lift_IO @@ IO.failwith @@ "Expected file with LIGO code, got: " ^ Path.to_string path
  | Some s -> return s


(**
Use doc info from cache. In case it's not available, return default value.
This can happen only if we obtained a request for document that was not opened, so
it's ok to return things like [None] in this case.
In case doc was not processed yet launches doc processing and sends diagnostics
*)
let with_cached_doc
    ~(default : 'a)
    (* Default value in case cached doc not found *)
      (path : Path.t)
    (f : Ligo_interface.file_data -> 'a Handler.t)
    : 'a Handler.t
  =
  let@ docs = ask_docs_cache in
  match Docs_cache.find docs path with
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
      let@ () = send_debug_msg "Found definitions, hierarchy and TZIP-16 storages" in
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
      let@ () = send_debug_msg "Recalculating caches" in
      let@ last_project_dir = ask_last_project_dir in
      let project_root = !last_project_dir in
      let@ syntax = get_syntax_exn path in
      let@ ({ definitions; potential_tzip16_storages; _ } as defs_and_diagnostics) =
        with_run_in_IO
        @@ fun { unlift_IO } ->
        Ligo_interface.get_defs_and_diagnostics
          ~project_root
          ~code
          ~logger:(fun ~type_ msg -> unlift_IO @@ send_log_msg ~type_ msg)
          path
      in
      let hierarchy = lazy (Def.Hierarchy.create definitions) in
      let parse_error_ranges =
        List.fold defs_and_diagnostics.errors ~init:[] ~f:(fun acc -> function
          | `Parser_tracer (`Parsing { value = _; region }) ->
            Range.of_region region :: acc
          | _ -> acc)
      in
      let@ () =
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
      let@ max_number_of_problems = fmap (fun x -> x.max_number_of_problems) ask_config in
      let diags_by_file =
        let simple_diags = Diagnostics.get_diagnostics path defs_and_diagnostics in
        Diagnostics.partition_simple_diagnostics
          path
          (Some max_number_of_problems)
          simple_diags
      in
      (* Corner case: clear diagnostics for this file in case there are none. *)
      let@ () =
        let uri = DocumentUri.of_path path in
        if List.Assoc.mem diags_by_file ~equal:DocumentUri.equal uri
        then pass
        else send_diagnostic uri []
      in
      let@ () = iter diags_by_file ~f:(Simple_utils.Utils.uncurry send_diagnostic) in
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


(* Calculates definitions and scopes for given file,
   saving them to cache and sending diagnostics to the client.
   If the document was already processed - does nothing *)
let process_doc (path : Path.t) : unit Handler.t =
  with_cached_doc path ~default:() (fun _ -> pass)


let with_cached_doc_pure
    ~(default : 'a)
    (path : Path.t)
    (f : Ligo_interface.file_data -> 'a)
    : 'a Handler.t
  =
  let f' = return <@ f in
  with_cached_doc path ~default f'


let with_code (path : Path.t) ~(default : 'a) (f : string -> 'a Handler.t) : 'a Handler.t =
  let@ docs = ask_docs_cache in
  match Docs_cache.find docs path with
  | Some file_data -> f file_data.code
  | None -> return default


(** Like with_cached_doc, but parses a CST from code. If `strict` is passed, error
    recovery for parsing is disabled, so default arg will be returned in case of
    any parse error.
    `on_error` allows to display a pop-up instead of just adding a debug message
    in case of an error. This is helpful for displaying to user why his request failed.
    We should not display pop-up when automated (e.g. document link) request failed,
    but should when e.g. formatting failed because of syntax error.
*)
let with_cst
    ~(default : 'a)
    ?(strict = false)
    ?(on_error : string -> unit handler =
      fun err -> send_debug_msg @@ "Unable to get CST: " ^ err)
    (path : Path.t)
    (f : Dialect_cst.t -> 'a Handler.t)
    : 'a Handler.t
  =
  with_code ~default path
  @@ fun code ->
  let@ syntax = get_syntax_exn path in
  match
    Ligo_api.Dialect_cst.get_cst
      ?project_root:(Option.map ~f:Path.to_string @@ Project_root.get_project_root path)
      ~preprocess_define:(* TODO: extract from project root ? *) []
      ~strict
      ~file:(Path.to_string path)
      syntax
      (Caml.Buffer.of_seq (Caml.String.to_seq code))
  with
  | Error err ->
    let@ () = on_error err in
    return default
  | Ok cst -> f cst


let update_project_root : Path.t option -> unit Handler.t =
 fun path ->
  let@ last_project_dir = ask_last_project_dir in
  let@ mod_res = ask_mod_res in
  return
  @@
  match path with
  | None ->
    last_project_dir := None;
    mod_res := None
  | Some path ->
    last_project_dir := Some path;
    mod_res := Preprocessor.ModRes.make (Path.to_string path)
