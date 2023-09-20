module LigoRC = Cli_helpers.LigoRC

type data =
  { name : string
  ; password : string
  ; email : string option
  }
[@@deriving to_yojson]

let login_url ~base_url user =
  Uri.with_path base_url (Format.sprintf "/-/api/-/user/org.couchdb.user:%s" user)


let http ~uri ~authorization ~user ~pass ~email =
  let open Cohttp_lwt_unix in
  let headers =
    Cohttp.Header.of_list
      [ "authorization", authorization; "content-type", "application/json" ]
  in
  let body =
    { name = user; password = pass; email }
    |> data_to_yojson
    |> Yojson.Safe.to_string
    |> Cohttp_lwt.Body.of_string
  in
  Client.put ~headers ~body uri


type server_success_response =
  { token : string
  ; ok : string
  }

let extract_success_response response =
  let module Util = Yojson.Safe.Util in
  let ok = response |> Util.member "ok" |> Util.to_string in
  let token = response |> Util.member "token" |> Util.to_string in
  { token; ok }


let handle_server_response ~update_token response body =
  let open Cohttp_lwt in
  let body = Lwt_main.run (Body.to_string body) in
  let code = Response.status response in
  match code with
  | `Unauthorized -> Error ("Access Denied: Wrong username or password", "")
  | `Created ->
    let body_json = Yojson.Safe.from_string body in
    let { token; ok } = extract_success_response body_json in
    let () = LigoRC.write (update_token ~token) in
    Ok (ok, "")
  | `Bad_gateway | `Service_unavailable | `Gateway_timeout ->
    Error ("Registry seems down. Contact the developers", "")
  | _ -> Error (body, "")


let ping ~email ~user ~pass ~ligo_registry ~ligorc_path =
  let registry_key = LigoRC.registry_key ligo_registry in
  let authorization =
    let token = Base64.encode_exn (Format.sprintf "%s:%s" user pass) in
    Format.sprintf "Basic %s" token
  in
  let login_url = login_url ~base_url:ligo_registry user in
  let ligorc = LigoRC.read ~ligorc_path in
  let update_token = LigoRC.update_token ~registry_key ligorc in
  let response, body =
    Lwt_main.run (http ~uri:login_url ~authorization ~user ~pass ~email)
  in
  handle_server_response ~update_token response body


let login ~ligo_registry ~ligorc_path =
  let prompt () =
    let open Lwt_result.Syntax in
    let* user = Prompt.prompt ~msg:"Username: " in
    let* password = Prompt.prompt_sensitive ~msg:"Password: " in
    Lwt_result.return (user, password)
  in
  match Lwt_main.run @@ prompt () with
  | Ok (user, pass) -> ping ~user ~email:None ~pass ~ligo_registry ~ligorc_path
  | Error (Prompt.Unknown_error e) -> Error (Exn.to_string e, "")
  | Error Prompt.Cancelled -> Error ("Aborted", "")
  | Error Prompt.Not_tty -> Error ("Not an interactive terminal", "")


(* (Caml.Sys.getenv "LIGO_USERNAME", Caml.Sys.getenv "LIGO_PASSWORD") *)

let create ~ligo_registry ~ligorc_path =
  let prompt () =
    let open Lwt_result.Syntax in
    let* user = Prompt.prompt ~msg:"Username: " in
    let* password = Prompt.prompt_sensitive ~msg:"Password: " in
    let* email = Prompt.prompt ~msg:"Email: " in
    Lwt_result.return (user, password, email)
  in
  match Lwt_main.run @@ prompt () with
  | Ok (user, pass, email) ->
    ping ~email:(Some email) ~user ~pass ~ligo_registry ~ligorc_path
  | Error (Prompt.Unknown_error e) -> Error (Exn.to_string e, "")
  | Error Prompt.Cancelled -> Error ("Aborted", "")
  | Error Prompt.Not_tty -> Error ("Not an interactive terminal", "")
