module LigoRC = Cli_helpers.LigoRC

(* TODO: prompt user for username, password & email (scanf??) *)
let prompt () = "foo1", "bar", "foo@bar.com"

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

let extract_token response =
  let module Util = Yojson.Safe.Util in
  let token = response |> Util.member "token" |> Util.to_string  in
  token

let create_or_login ~ligo_registry ~ligorc_path =
  let registry_key = LigoRC.registry_key ligo_registry in
  let ligorc = LigoRC.read ~ligorc_path in
  let token_opt = LigoRC.get_token ligorc ~registry_key in
  let user, pass, email = prompt () in
  let authorization = match token_opt with
    Some token -> Format.sprintf "Bearer %s" token
  | None -> 
    let token = Base64.encode_exn (Format.sprintf "%s:%s" user pass) in
    Format.sprintf "Basic %s" token
  in
  let login_url = login_url ~base_url:ligo_registry user in
  let open Cohttp in
  let open Cohttp_lwt in

  let response, body = Lwt_main.run (http ~uri:login_url ~authorization ~user ~pass) in
  let body = Lwt_main.run (Body.to_string body) in
  let body_json = Yojson.Safe.from_string body in
  let token = extract_token body_json in
  let ligorc = LigoRC.update_token ~registry_key ~token ligorc in 
  let () = LigoRC.write ~registry_key ~token ligorc in
  (* TODO: better error & success message *)
  let code = response |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (response |> Response.headers |> Header.to_string);
  Printf.printf "Body: %s\n" body;
  (* TODO: use _body for better errors *)
  Ok("", "")