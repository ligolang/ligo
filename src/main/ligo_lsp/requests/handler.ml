open Linol_lwt
open Linol_lwt.Jsonrpc2
open Utils
module Hashtbl = Caml.Hashtbl

type config =
  { max_number_of_problems : int
  ; logging_verbosity : MessageType.t
  }

type notify_back_mockable =
  | Normal of notify_back
  | Mock of Jsonrpc2.Diagnostic.t list ref (* FIXME: collect logs for tests *)

type handler_env =
  { notify_back : notify_back_mockable
  ; config : config
  ; docs_cache : (DocumentUri.t, Ligo_interface.file_data) Hashtbl.t
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
let ask_config : config Handler.t = fmap (fun x -> x.config) ask

let ask_docs_cache : (DocumentUri.t, Ligo_interface.file_data) Hashtbl.t Handler.t =
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
  | Normal nb ->
    let@ { logging_verbosity; _ } = ask_config in
    if Caml.(type_ <= logging_verbosity)
    then lift_IO @@ nb#send_log_msg ~type_ s
    else return ()
  | Mock _ -> return ()


let send_diagnostic (s : Jsonrpc2.Diagnostic.t list) : unit Handler.t =
  let@ nb = ask_notify_back in
  match nb with
  | Normal nb -> lift_IO (nb#send_diagnostic s)
  | Mock mock_ref ->
    mock_ref := s @ !mock_ref;
    return ()


let send_debug_msg : string -> unit Handler.t = send_log_msg ~type_:MessageType.Log

let send_message ?(type_ : MessageType.t = Info) (message : string) : unit Handler.t =
  let@ nb = ask_notify_back in
  match nb with
  | Normal nb ->
    lift_IO
      (nb#send_notification @@ ShowMessage (ShowMessageParams.create ~message ~type_))
  | Mock _ -> return ()

    
let with_cached_doc
    ?(return_default_if_no_info = true)
    (uri : DocumentUri.t)
    (default : 'a) (* Default value in case cached doc not found *)
    (f : Ligo_interface.file_data -> 'a Handler.t)
    : 'a Handler.t
  =
  let@ docs = ask_docs_cache in
  match Hashtbl.find_opt docs uri with
  | Some file_data ->
    if (not return_default_if_no_info) || file_data.get_scope_info.has_info
    then f file_data
    else return default
  | None -> return default


let with_cached_doc_pure
    ?return_default_if_no_info
    (uri : DocumentUri.t)
    (default : 'a)
    (f : Ligo_interface.file_data -> 'a)
    : 'a Handler.t
  =
  let f' = return @. f in
  with_cached_doc ?return_default_if_no_info uri default f'


let with_cst
    ?(strict = false)
    ?(on_error : string -> unit handler =
      fun err -> send_debug_msg @@ "Unable to get CST: " ^ err)
    (uri : DocumentUri.t)
    (default : 'a)
    (f : dialect_cst -> 'a Handler.t)
    : 'a Handler.t
  =
  with_cached_doc ~return_default_if_no_info:false uri default
  @@ fun { syntax; code; _ } ->
  match get_cst ~strict syntax code with
  | Error err ->
    let@ () = on_error err in
    return default
  | Ok cst -> f cst
