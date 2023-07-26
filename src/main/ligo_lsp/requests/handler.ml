open Lsp_helpers
module Path_hashtbl = Hashtbl.Make (Path)

module Docs_cache = struct
  include Path_hashtbl (* So we can call e.g. [Docs_cache.find] *)

  type t = Ligo_interface.file_data Path_hashtbl.t

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
  ; deprecated : bool
        (** Enable support for the deprecated PascaLIGO syntax. Defaults to [false]. *)
  ; max_line_width : int option
        (** Override the max line width for formatted file (80 by default) *)
  }

(** We can send diagnostics to user or just save them to list in case of testing *)
type notify_back_mockable =
  | Normal of DocumentUri.t * Linol_lwt.Jsonrpc2.notify_back
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


(** We can run things from [Linol_lwt.IO] monad here *)
let lift_IO (m : 'a IO.t) : 'a Handler.t = Handler (fun _ -> m)

(** Get env inside monad computation *)
let ask : handler_env Handler.t = Handler IO.return

let ask_notify_back : notify_back_mockable Handler.t = fmap (fun x -> x.notify_back) ask
let ask_config : config Handler.t = fmap (fun x -> x.config) ask
let ask_docs_cache : Docs_cache.t Handler.t = fmap (fun x -> x.docs_cache) ask

(** Sequencing handlers *)

let iter (xs : 'a list) ~f:(h : 'a -> unit Handler.t) : unit Handler.t =
  let rec go = function
    | [] -> return ()
    | x :: xs -> bind (h x) (fun () -> go xs)
  in
  go xs


(** Conditional computations *)

let when_ (b : bool) (m : unit Handler.t) : unit Handler.t = if b then m else return ()

let when_some (m_opt : 'a option) (f : 'a -> 'b Handler.t) : 'b option Handler.t =
  match m_opt with
  | Some m -> fmap Option.some (f m)
  | None -> return None


let when_some_ (m_opt : 'a option) (f : 'a -> unit Handler.t) : unit Handler.t =
  match m_opt with
  | Some m -> f m
  | None -> return ()


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


let send_log_msg ~(type_ : MessageType.t) (s : string) : unit Handler.t =
  let@ nb = ask_notify_back in
  match nb with
  | Normal (_uri, nb) ->
    let@ { logging_verbosity; _ } = ask_config in
    if Caml.(type_ <= logging_verbosity)
    then lift_IO @@ nb#send_log_msg ~type_ s
    else return ()
  | Mock _ -> return ()


(** Send diagnostics (errors, warnings) to the LSP client. *)
let send_diagnostic (uri : DocumentUri.t) (s : Diagnostic.t list) : unit Handler.t =
  let@ nb = ask_notify_back in
  match nb with
  | Normal (original_uri, nb) ->
    let () = nb#set_uri uri in
    let@ () = lift_IO (nb#send_diagnostic s) in
    let () = nb#set_uri original_uri in
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
  | Normal (_uri, nb) ->
    lift_IO
      (nb#send_notification @@ ShowMessage (ShowMessageParams.create ~message ~type_))
  | Mock _ -> return ()


(**
Use doc info from cache. In case it's not available, return default value.
Also returns default value if `get_scope` for this file fails, unless
`return_default_if_no_info = false` was specified
*)
let with_cached_doc
    (path : Path.t)
    (default : 'a) (* Default value in case cached doc not found *)
    (f : Ligo_interface.file_data -> 'a Handler.t)
    : 'a Handler.t
  =
  let@ docs = ask_docs_cache in
  match Docs_cache.find docs path with
  | Some file_data -> f file_data
  | None -> return default


let with_cached_doc_pure
    (path : Path.t)
    (default : 'a)
    (f : Ligo_interface.file_data -> 'a)
    : 'a Handler.t
  =
  let f' = return <@ f in
  with_cached_doc path default f'


(** Like with_cached_doc, but parses a CST from code. If `strict` is passed, error
    recovery for parsing is disabled, so default arg will be returned in case of
    any parse error.
    `on_error` allows to display a pop-up instead of just adding a debug message
    in case of an error. This is helpful for displaying to user why his request failed.
    We should not display pop-up when automated (e.g. document link) request failed,
    but should when e.g. formatting failed because of syntax error.
*)
let with_cst
    ?(strict = false)
    ?(on_error : string -> unit handler =
      fun err -> send_debug_msg @@ "Unable to get CST: " ^ err)
    (path : Path.t)
    (default : 'a)
    (f : Dialect_cst.t -> 'a Handler.t)
    : 'a Handler.t
  =
  with_cached_doc path default
  @@ fun { syntax; code; _ } ->
  match Dialect_cst.get_cst ~strict ~file:path syntax code with
  | Error err ->
    let@ () = on_error err in
    return default
  | Ok cst -> f cst
