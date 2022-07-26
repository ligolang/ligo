module LigoRC = Cli_helpers.LigoRC

class read_username term = object(self)
  inherit LTerm_read_line.read_line () as super
  inherit [Zed_string.t] LTerm_read_line.term term

  method! send_action = function
  | LTerm_read_line.Break -> exit 0
  | action -> super#send_action action

  method! show_box = false

  initializer
    self#set_prompt (Lwt_react.S.const (LTerm_text.of_utf8 "Username: "))
end

class read_password term = object(self)
  inherit LTerm_read_line.read_password () as super
  inherit [Zed_string.t] LTerm_read_line.term term

  method! send_action = function
    | LTerm_read_line.Break -> exit 0
    | action -> super#send_action action

  initializer
    self#set_prompt (Lwt_react.S.const (LTerm_text.of_utf8 "Password: "))
end

let prompt () =
  let open Lwt.Syntax in
  let* () = LTerm_inputrc.load () in
  let* term = Lazy.force LTerm.stdout in
  let* user = (new read_username term)#run in
  let* pass = (new read_password term)#run in
  Lwt.return (user, pass)

let prompt () =
  let u, p = Lwt_main.run (prompt ()) in
  Zed_string.to_utf8 u, Zed_string.to_utf8 p

type data =
  { name     : string
  ; password : string 
  } [@@deriving to_yojson]

let login_url ~base_url user =
  Format.sprintf "%s/-/user/org.couchdb.user:%s" base_url user

let http ~uri ~authorization ~user ~pass =
  let open Cohttp_lwt_unix in
  let uri = Uri.of_string uri in
  let headers = Cohttp.Header.of_list [
    ("authorization", authorization) ;
    ("content-type", "application/json") ;
  ] in
  let body = { name = user ; password = pass }
    |> data_to_yojson 
    |> Yojson.Safe.to_string 
    |> Cohttp_lwt.Body.of_string in
  Client.put ~headers ~body uri

type server_success_response = { token : string ; ok : string }

let extract_success_response response =
  let module Util = Yojson.Safe.Util in
  let ok = response |> Util.member "ok" |> Util.to_string in
  let token = response |> Util.member "token" |> Util.to_string in
  { token ; ok }

let handle_server_response ~update_token response body =
  let open Cohttp_lwt in
  let body = Lwt_main.run (Body.to_string body) in
  let code = Response.status response in
  match code with
    `Unauthorized -> Error ("Access Denied: Wrong username or password", "")
  | `Created ->
    let body_json = Yojson.Safe.from_string body in
    let { token ; ok } = extract_success_response body_json in
    let () = LigoRC.write (update_token ~token) in
    Ok(ok, "")
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout -> Error ("Registry seems down. Contact the developers", "")
  | _ -> Error (body, "")

let create_or_login ~ligo_registry ~ligorc_path =
  let registry_key = LigoRC.registry_key ligo_registry in
  let ligorc = LigoRC.read ~ligorc_path in
  let token_opt = LigoRC.get_token ligorc ~registry_key in
  let user, pass = prompt () in
  let authorization = match token_opt with
    Some token -> Format.sprintf "Bearer %s" token
  | None -> 
    let token = Base64.encode_exn (Format.sprintf "%s:%s" user pass) in
    Format.sprintf "Basic %s" token
  in
  let login_url = login_url ~base_url:ligo_registry user in
  let update_token = LigoRC.update_token ~registry_key ligorc in
  let response, body = Lwt_main.run (http ~uri:login_url ~authorization ~user ~pass) in
  handle_server_response ~update_token response body