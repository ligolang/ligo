module LigoRC = Cli_helpers.LigoRC
open Registry.Response
open Utils
module Uri = Package_management_external_libs.Ligo_uri

let handle_server_response ~name response body =
  let open Cohttp_lwt in
  let body = Lwt_main.run (Body.to_string body) in
  let code = Response.status response in
  match code with
  | `Created -> Ok ("", "")
  | `Unauthorized ->
    Error
      ( Format.sprintf "\n%s already exists and you don't seem to have access to it." name
      , "" )
  | `Bad_gateway | `Service_unavailable | `Gateway_timeout ->
    Error ("\nRegistry seems down. Contact the developers", "")
  | _ -> Error (body, "")


(* [delete_package] makes a HTTP delete request to the [ligo_registry]
   used [token] for authorization *)
let delete_package ~name ~ligo_registry ~token =
  let open Cohttp_lwt_unix in
  let endpoint_uri = Format.sprintf "/-/api/%s" name in
  let uri = Uri.with_path ligo_registry endpoint_uri in
  let headers =
    Cohttp.Header.of_list
      [ "referer", "unpublish"
      ; "authorization", Format.sprintf "Bearer %s" token
      ; "Content-Type", "application/json"
      ]
  in
  let r = Client.delete ~headers uri in
  let response, body = Lwt_main.run r in
  handle_server_response ~name response body


let unpublish_package_version ~name ~body ~ligo_registry ~token ~tarball_name ~revision =
  let open Lwt.Infix in
  let open Cohttp_lwt_unix in
  let ( let* ) x f = Result.bind x ~f in
  let endpoint_uri = Format.sprintf "/-/api/%s" name in
  let uri = Uri.with_path ligo_registry endpoint_uri in
  let body =
    body |> GetBody.to_yojson |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
  in
  let r =
    Cohttp_lwt.Body.length body
    >>= fun (content_size, body) ->
    let headers =
      Cohttp.Header.of_list
        [ "referer", "unpublish"
        ; "authorization", Format.sprintf "Bearer %s" token
        ; "Content-Type", "application/json"
        ; "Content-Length", Int64.to_string content_size
        ]
    in
    Client.put ~headers ~body uri
  in
  let response, body = Lwt_main.run r in
  let* _, _ = handle_server_response ~name response body in
  let headers =
    Cohttp.Header.of_list
      [ "referer", "unpublish"; "authorization", Format.sprintf "Bearer %s" token ]
  in
  let endpoint_uri = Format.sprintf "/-/api/%s/-/%s/-rev/%s" name tarball_name revision in
  let uri = Uri.with_path ligo_registry endpoint_uri in
  let r = Client.delete ~headers uri in
  let response, body_str = Lwt_main.run r in
  handle_server_response ~name response body_str


(* The function [get_auth_token] reads as .ligorc file and extracts the 
   auth-token for the requested [ligo_registry] *)
let get_auth_token ~ligorc_path ligo_registry =
  let ligorc = LigoRC.read ~ligorc_path in
  let registry_key = LigoRC.registry_key ligo_registry in
  let token = LigoRC.get_token ~registry_key ligorc in
  match token with
  | Some token -> Ok token
  | None -> Error ("\nUser not logged in.\nHint: Use ligo login or ligo add-user.", "")


let get_package ~(name : string) ligo_registry : (GetBody.t, string * string) result Lwt.t
  =
  let open Lwt.Infix in
  let handle_ok body =
    Cohttp_lwt.Body.to_string body
    >>= fun body_str ->
    match body_str |> Yojson.Safe.from_string |> GetBody.of_yojson with
    | Ok v -> Ok v |> Lwt.return
    | Error e -> Error (e, "") |> Lwt.return
  in
  let endpoint_uri = Format.sprintf "/-/api/%s" name in
  let uri = Uri.with_path ligo_registry endpoint_uri in
  Cohttp_lwt_unix.Client.get uri
  >>= fun (response, body) ->
  match Cohttp.Response.status response with
  | `Bad_gateway | `Service_unavailable | `Gateway_timeout ->
    Lwt.return @@ Error ("\nRegistry seems down. Contact the developers", "")
  | `OK -> handle_ok body
  | `Not_found ->
    let msg = Printf.sprintf "Package %s not found" name in
    Lwt.return @@ Error (msg, "")
  | _ -> Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return @@ Error (body, "")


let unpublish ~name ~version ~ligo_registry ~ligorc_path =
  let ( let* ) x f = Result.bind x ~f in
  let* token =
    with_logging ~before:"Checking auth token" (fun () ->
        get_auth_token ~ligorc_path ligo_registry)
  in
  match name, version with
  | Some name, None ->
    with_logging ~before:"Deleting package completely" (fun () ->
        delete_package ~name ~token ~ligo_registry)
  | Some name, Some version ->
    let tarball_name = get_tarball_name ~name ~version in
    (match Lwt_main.run @@ get_package ~name ligo_registry with
    | Error (e, w) ->
      let e = Printf.sprintf "Failed to retrieve metadata for %s. Reason %s" name e in
      Error (e, w)
    | Ok { name; attachments; versions; readme; description; revision; _ } ->
      (match
         SMap.cardinal versions, SMap.exists (fun k _ -> String.equal k version) versions
       with
      | 1, true ->
        let before =
          Printf.sprintf
            "Package %s has only one version %s. Unpublishing the entire package.."
            name
            version
        in
        with_logging ~before (fun () -> delete_package ~name ~token ~ligo_registry)
      | 1, false ->
        let msg = Printf.sprintf "Package %s does not have version %s. " name version in
        Error (msg, "")
      | _, _ ->
        let f k _ = not (String.equal k version) in
        let versions = SMap.filter f versions in
        let latest_version =
          match
            versions
            |> SMap.bindings
            |> List.filter_map ~f:(fun (v, _) -> Semver.of_string v)
            |> List.sort ~compare:(fun x y ->
                   Semver.compare x y * -1 (* inverts the order *))
          with
          | [] -> "" (* Could not find a suitable version to set as latest *)
          | (major, minor, patch) :: _ -> Printf.sprintf "%d.%d.%d" major minor patch
        in
        let dist_tags = { latest = latest_version } in
        (match revision with
        | None -> Error ("Internal error: revision missing from registry response", "")
        | Some revision ->
          with_logging ~before:"Unpublishing package" (fun () ->
              let body : GetBody.t =
                GetBody.make
                  ~attachments
                  ~name
                  ~dist_tags
                  ~versions
                  ~readme
                  ~description
                  ~revision:(Some revision)
              in
              unpublish_package_version
                ~revision
                ~name
                ~body
                ~token
                ~ligo_registry
                ~tarball_name))))
  | _, _ -> Error ("Atleast name must be provided to unpublish command", "")
