open Package_management

(* [publish] makes a HTTP put request to the [ligo_registry] with the [body] 
   and used [token] for authorization *)
let publish ~ligo_registry ~manifest ~body ~token =
  let open Cohttp_lwt_unix in
  let LigoManifest.{ name; _ } = manifest in
  let uri = Uri.of_string (Format.sprintf "%s/%s" ligo_registry name) in
  let body =
    body |> PutBody.to_yojson |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
  in
  let body_headers =
    Lwt.bind (Cohttp_lwt.Body.length body) (fun (content_size, body) ->
        let headers =
          Cohttp.Header.of_list
            [ "referer", "publish"
            ; "authorization", Format.sprintf "Bearer %s" token
            ; "Content-Type", "application/json"
            ; "Content-Length", Int64.to_string content_size
            ]
        in
        Lwt.return (body, headers))
  in
  let r = Lwt.bind body_headers (fun (body, headers) -> Client.put ~headers ~body uri) in
  let response, body = Lwt_main.run r in
  handle_server_response ~name:manifest.name response body


let publish ~ligo_registry ~ligorc_path ~project_root ~dry_run =
  let* manifest =
    with_logging ~before:"Reading manifest" (fun () -> read_manifest ~project_root)
  in
  let* () =
    with_logging ~before:"Validating manifest file" (fun () ->
        validate_manifest manifest)
  in
  let* project_root =
    with_logging ~before:"Finding project root" (fun () -> get_project_root project_root)
  in
  let ligoignore_path = Filename.concat project_root ".ligoignore" in
  let ligoignore = LigoIgnore.matches @@ LigoIgnore.read ~ligoignore_path in
  let* package_stats =
    with_logging ~before:"Packing tarball" (fun () ->
        pack ~project_root ~ligoignore ~manifest)
  in
  let () = show_stats package_stats in
  if dry_run
  then Ok ("", "")
  else (
    let attachments = PutAttachments.make ~package_stats in
    let LigoManifest.{ name; description; version; readme; _ } = manifest in
    let v = Version.make ~ligo_registry ~package_stats ~manifest in
    let version_str = Semver.to_string version in
    let versions = SMap.add version_str v SMap.empty in
    let dist_tags = { latest = version_str } in
    let body =
      PutBody.make
        ~dist_tags
        ~versions
        ~revision:None
        ~description
        ~name
        ~readme
        ~attachments
    in
    let* token =
      with_logging ~before:"Checking auth token" (fun () ->
          get_auth_token ~ligorc_path ligo_registry)
    in
    with_logging ~before:"Uploading package" (fun () ->
        publish ~token ~body ~ligo_registry ~manifest))
