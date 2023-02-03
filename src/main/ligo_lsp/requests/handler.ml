open Linol_lwt
open Linol_lwt.Jsonrpc2
module Hashtbl = Caml.Hashtbl

type notify_back_mockable =
  | Normal of notify_back
  | Mock of Jsonrpc2.Diagnostic.t list ref

type handler_env =
  { notify_back : notify_back_mockable
  ; debug : bool
  ; docs_cache : (DocumentUri.t, Ligo_interface.get_scope_info) Hashtbl.t
  }

type 'a handler = Handler of (handler_env -> 'a IO.t)

module Handler = struct
  type 'a t = 'a handler
end

let return (a : 'a) : 'a Handler.t = Handler (fun _ -> IO.return a)

type 'a t = 'a Handler.t

let un_handler (Handler a : 'a Handler.t) : handler_env -> 'a IO.t = a
let run_handler (env : handler_env) (r : 'a Handler.t) : 'a IO.t = un_handler r env

let bind (Handler d : 'a Handler.t) (f : 'a -> 'b Handler.t) : 'b Handler.t =
  Handler
    (fun env ->
      let* p = d env in
      un_handler (f p) env)


let ( let@ ) = bind

let fmap (f : 'a -> 'b) (x : 'a Handler.t) : 'b Handler.t =
  let@ x' = x in
  return @@ f x'


let fmap_to (x : 'a Handler.t) (f : 'a -> 'b) : 'b Handler.t = fmap f x
let lift_IO (m : 'a IO.t) : 'a Handler.t = Handler (fun _ -> m)
let ask : handler_env Handler.t = Handler IO.return
let ask_notify_back : notify_back_mockable Handler.t = fmap (fun x -> x.notify_back) ask
let ask_debug : bool Handler.t = fmap (fun x -> x.debug) ask

let ask_docs_cache : (DocumentUri.t, Ligo_interface.get_scope_info) Hashtbl.t Handler.t =
  fmap (fun x -> x.docs_cache) ask


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
  | Some m -> fmap Fun.id (f m)
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
  | Normal nb -> lift_IO (nb#send_log_msg ~type_ s)
  | Mock _ -> return ()


let send_diagnostic (s : Jsonrpc2.Diagnostic.t list) : unit Handler.t =
  let@ nb = ask_notify_back in
  match nb with
  | Normal nb -> lift_IO (nb#send_diagnostic s)
  | Mock mock_ref ->
    mock_ref := s @ !mock_ref;
    return ()


let send_debug_msg (s : string) : unit Handler.t =
  let@ debug = ask_debug in
  when_ debug @@ send_log_msg ~type_:MessageType.Info s


let with_cached_doc
    :  DocumentUri.t -> 'a (* Default value in case cached doc not found *)
    -> (Ligo_interface.get_scope_info -> 'a Handler.t) -> 'a Handler.t
  =
 fun uri default f ->
  let@ docs = ask_docs_cache in
  match Hashtbl.find_opt docs uri with
  | Some get_scope_info ->
    if get_scope_info.has_info
    then f get_scope_info
    else return default
  | None -> return default
