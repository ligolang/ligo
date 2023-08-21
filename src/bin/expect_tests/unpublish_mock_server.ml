open Lwt
open Cohttp
open Cohttp_lwt_unix

exception Exit

exception
  Failed_to_retrieve_package of
    { code : string
    ; body : string
    ; headers : string
    }

let dao_cameligo_body = ref Pkg_two_versions.body
let owner = "foo"
let owner_token = "foo_token"

let server_lwt port =
  let unrecognised_request meth path =
    let meth_str = Code.string_of_method meth in
    let body = Printf.sprintf "Unrecognised %s request to %s" meth_str path in
    Server.respond_string ~status:`OK ~body ()
  in
  let send_dao_cameligo_body () =
    let open Caml in
    let body = !dao_cameligo_body in
    if body != ""
    then Server.respond_string ~status:`OK ~body ()
    else Server.respond_string ~status:`Not_found ~body:"{}" ()
  in
  let update_dao_cameligo body =
    dao_cameligo_body := body;
    Server.respond_string ~status:`Created ~body:"" ()
  in
  let delete_dao_cameligo () =
    if String.equal !dao_cameligo_body ""
    then Server.respond_string ~status:`Not_found ~body:"Package not found" ()
    else (
      dao_cameligo_body := "";
      Server.respond_string ~status:`Created ~body:"{ \"ok\": \"package removed\" }" ())
  in
  let delete_dao_cameligo_tarball () =
    Server.respond_string ~status:`Created ~body:"{ \"ok\": \"package removed\" }" ()
  in
  let check_auth headers callback =
    let authorization = Caml.Option.get @@ Header.get headers "Authorization" in
    if String.equal authorization "Bearer foo_token"
       || String.equal authorization "Basic foo_token"
    then callback ()
    else (
      let () = print_endline authorization in
      Server.respond_string ~status:`Unauthorized ~body:"Not authorised" ())
  in
  let process_request _conn req body =
    let path = req |> Request.uri |> Uri.path in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers in
    body
    |> Cohttp_lwt.Body.to_string
    >>= fun body ->
    (* let meth_str = Code.string_of_method meth in *)
    (* Printf.printf "%s %s" meth_str body; *)
    match meth, path with
    | `GET, "/-/api/@foo/bar-pkg" -> send_dao_cameligo_body ()
    | `PUT, "/-/api/@foo/bar-pkg" ->
      check_auth headers @@ fun () -> update_dao_cameligo body
    | `DELETE, "/-/api/@foo/bar-pkg" -> check_auth headers @@ delete_dao_cameligo
    | `DELETE, "/-/api/@foo/bar-pkg/-/@foo/bar-pkg-1.0.5.tgz/-rev/17-cf860d9e8aeb5547" ->
      check_auth headers @@ delete_dao_cameligo_tarball
    | `GET, "/exit" -> raise Exit
    | _ -> unrecognised_request meth path
  in
  let server = Server.make ~callback:process_request () in
  let run = Server.create ~mode:(`TCP (`Port port)) in
  run server


let test () =
  let port = 8000 in
  Lwt_main.run @@ server_lwt port;
  Printf.printf "Running server on port %d" port;
  Out_channel.newline Out_channel.stdout


let get_package () =
  let uri_str = "/-/api/@foo/bar-pkg" in
  let uri = Uri.of_string uri_str in
  let body =
    Client.get uri
    >>= fun (resp, body) ->
    body
    |> Cohttp_lwt.Body.to_string
    >|= fun body ->
    match resp |> Response.status with
    | `OK -> body
    | code ->
      let code = Cohttp.Code.string_of_status code in
      let headers = resp |> Response.headers |> Header.to_string in
      raise (Failed_to_retrieve_package { code; body; headers })
  in
  Lwt_main.run body
