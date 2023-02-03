module Hashtbl = Caml.Hashtbl

(** We can send diagnostics to user or just save them to list in case of testing *)
type notify_back_mockable =
  | Normal of Linol_lwt.Jsonrpc2.notify_back
  | Mock of Linol_lwt.Diagnostic.t list ref

(** Enviroment availiable in Handler monad *)
type handler_env =
  { notify_back : notify_back_mockable
  ; debug : bool
  ; docs_cache : (Lsp.Uri.t, Ligo_interface.get_scope_info) Hashtbl.t
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
val ask_docs_cache : (Lsp.Uri.t, Ligo_interface.get_scope_info) Hashtbl.t Handler.t

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

(** Use doc info from cache. In case it's not availiable, return default value. *)
val with_cached_doc
  :  Lsp.Uri.t
  -> 'a
  -> (Ligo_interface.get_scope_info -> 'a Handler.t)
  -> 'a Handler.t
