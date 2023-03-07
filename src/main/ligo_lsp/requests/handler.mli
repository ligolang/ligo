module Hashtbl = Caml.Hashtbl

(** We can send diagnostics to user or just save them to list in case of testing *)
type notify_back_mockable =
  | Normal of Linol_lwt.Jsonrpc2.notify_back
  | Mock of Linol_lwt.Diagnostic.t list ref

(** Enviroment availiable in Handler monad *)
type handler_env =
  { notify_back : notify_back_mockable
  ; debug : bool
  ; docs_cache : (Linol_lwt.DocumentUri.t, Ligo_interface.file_data) Hashtbl.t
  }

(** Handler monad : allows sending messages to user and reading docs cache *)
type 'a handler = Handler of (handler_env -> 'a Lwt.t)

module Handler : sig
  type 'a t = 'a handler
end

val return : 'a -> 'a Handler.t

type 'a t = 'a handler

val un_handler : 'a Handler.t -> handler_env -> 'a Lwt.t
val run_handler : handler_env -> 'a Handler.t -> 'a Lwt.t
val bind : 'a Handler.t -> ('a -> 'b Handler.t) -> 'b Handler.t
val ( let@ ) : 'a Handler.t -> ('a -> 'b Handler.t) -> 'b Handler.t

(** Handler monad is a functor *)
val fmap : ('a -> 'b) -> 'a Handler.t -> 'b Handler.t

(** Flipped fmap *)
val fmap_to : 'a Handler.t -> ('a -> 'b) -> 'b Handler.t

(** We can run things from Linol_lwt.IO monad here *)
val lift_IO : 'a Lwt.t -> 'a Handler.t

(** Get env inside monad computation *)
val ask : handler_env Handler.t

val ask_notify_back : notify_back_mockable Handler.t
val ask_debug : bool Handler.t

val ask_docs_cache
  : (Linol_lwt.DocumentUri.t, Ligo_interface.file_data) Hashtbl.t Handler.t

(** Conditional computations *)
val when_ : bool -> unit Handler.t -> unit Handler.t

val when_some : 'a option -> ('a -> 'b Handler.t) -> 'b option Handler.t
val when_some_ : 'a option -> ('a -> unit Handler.t) -> unit Handler.t
val when_some' : 'a option -> ('a -> 'b option Handler.t) -> 'b option Handler.t
val when_some_m : 'a option Handler.t -> ('a -> 'b Handler.t) -> 'b option Handler.t

val when_some_m'
  :  'a option Handler.t
  -> ('a -> 'b option Handler.t)
  -> 'b option Handler.t

val send_log_msg : type_:Linol_lwt.MessageType.t -> string -> unit Handler.t
val send_diagnostic : Linol_lwt.Diagnostic.t list -> unit Handler.t

(** Message will appear in log in case debug is enabled *)
val send_debug_msg : string -> unit Handler.t

(** Displays a pop-up message *)
val send_message : ?type_:Linol_lwt.MessageType.t -> string -> unit Handler.t

(**
Use doc info from cache. In case it's not availiable, return default value.
Also returns default value if `get_scope` for this file fails, unless
`return_default_if_no_info = false` was specified
*)
val with_cached_doc
  :  ?return_default_if_no_info:bool
  -> Linol_lwt.DocumentUri.t
  -> 'a
  -> (Ligo_interface.file_data -> 'a Handler.t)
  -> 'a Handler.t

val with_cached_doc_pure
  :  ?return_default_if_no_info:bool
  -> Linol_lwt.DocumentUri.t
  -> 'a
  -> (Ligo_interface.file_data -> 'a)
  -> 'a Handler.t

(** Like with_cached_doc, but parses a CST from code. If `strict` is passed, error
    recovery for parsing is disabled, so default arg will be returned in case of
    any parse error.
    `on_error` allows to display a pop-up instead of just adding a debug message
    in case of an error. This is helpful for displaying to user why his request failed.
    We should not display pop-up when automated (e.g. document link) request failed,
    but should when e.g. formatting failed because of syntax error.
*)
val with_cst
  :  ?strict:bool
  -> ?on_error:(string -> unit Handler.t)
  -> Linol_lwt.DocumentUri.t
  -> 'a
  -> (Utils.dialect_cst -> 'a Handler.t)
  -> 'a Handler.t
