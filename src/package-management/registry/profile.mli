type error =
  | Server_error
  | Invalid_token
  | Invalid_input

(** Creates a user's profile on the registry server. Doesn't return anything if successful. Returns [error] if the operation fails. It could either due to bad input from user, or a failure on the server. If so, the user can retry with the same input. Note: [create] doesn't take userName as the server infers it from session token. Sending it is useless. *)
val create
  :  ligo_registry:Uri.t
  -> token:string
  -> email:string
  -> fullname:string
  -> (unit, error) Lwt_result.t
