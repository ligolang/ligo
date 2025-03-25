open Prometheus
open Prometheus_format

module PushableCollectorRegistry = struct
  type t =
    { url : Uri.t
    ; mutable collectorRegistry : CollectorRegistry.t
    }

  let create url =
    { url = Uri.of_string url; collectorRegistry = CollectorRegistry.create () }

  let default : t option ref = ref None
  let set_default : string -> unit = fun url -> default := Some (create url)

  let get_default : unit -> t =
   fun () ->
    match !default with
    | Some t -> t
    | None -> failwith "Default registry hasn't been set."

  let clean t = t.collectorRegistry <- CollectorRegistry.create ()
  let ( let* ) = Lwt.bind
  let ( let+ ) v f = Lwt.map f v

  let push t =
    let* collected = CollectorRegistry.collect t.collectorRegistry in
    let open Cohttp_lwt_unix in
    let uri = t.url in
    let body =
      Fmt.to_to_string TextFormat_0_0_4.output collected |> Cohttp_lwt.Body.of_string
    in
    let* content_size, body = Cohttp_lwt.Body.length body in
    let headers =
      Cohttp.Header.of_list
        [ "Content-Type", "text/plain"
        ; "version", "0.0.4"
        ; "Content-Length", Int64.to_string content_size
        ]
    in
    let* response, body = Client.put ~headers ~body uri in
    clean t;
    let+ body = Cohttp_lwt.Body.to_string body in
    let code = Cohttp.Response.status response in
    match code with
    | #Cohttp.Code.success_status -> Ok "Metric successfully published"
    | _ ->
      Error
        (Format.sprintf "Bad Status: %s@.%s@." (Cohttp.Code.string_of_status code) body)

  let push t =
    Lwt.catch
      (fun () -> Lwt.pick [ Lwt_unix.timeout 3.0; push t ])
      (fun exn -> Lwt.return @@ Error (Printexc.to_string exn))
end
