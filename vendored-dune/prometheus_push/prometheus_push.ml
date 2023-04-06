open Prometheus
open Prometheus_format
open Lwt.Infix

module PushableCollectorRegistry = struct 
  
  type t = {
    url : Uri.t;
    mutable collectorRegistry : CollectorRegistry.t;
  }

  let create url = {
    url = Uri.of_string (url);
    collectorRegistry = CollectorRegistry.create ();
  }

  let default : t option ref = ref None

  let set_default : string -> unit =
    fun url -> 
      default := Some (create url)

  let get_default : unit -> t =
    fun () ->
      match !default with
      | Some t -> t
      | None -> failwith "Default registry hasn't been set."

  let clean t = 
    t.collectorRegistry <- CollectorRegistry.create ()

let handle_server_response response body =
  let open Cohttp_lwt in
  let body = Lwt_main.run (Body.to_string body) in
  print_string body; 
  let code = Response.status response in
  match code with
  | `Created -> Ok ("Metric successfully published", "")
  | _ -> Error (body, "")


  let push t =
    CollectorRegistry.collect t.collectorRegistry >|= fun collected ->
    let open Cohttp_lwt_unix in
      let uri = t.url in
      let body = Fmt.to_to_string TextFormat_0_0_4.output collected |> Cohttp_lwt.Body.of_string in 
      let body_headers =
      Lwt.bind (Cohttp_lwt.Body.length body) (fun (content_size, body) ->
          let headers =
            Cohttp.Header.of_list
                [
                "Content-Type", "text/plain"
              ; "version", "0.0.4"
              ; "Content-Length", Int64.to_string content_size
              ]
          in
          Lwt.return (body, headers))
    in
      let r = Lwt.bind body_headers (fun (body, headers) -> 
        Lwt.pick [ Lwt_unix.timeout 1.0;Client.put ~headers ~body uri]) in
      let response, body = Lwt_main.run r in
      clean t;
      handle_server_response response body
end
