open Lwt_result.Syntax

let endpoint_uri = "/-/api/-/whoami"

module Request = struct
  type t = unit [@@deriving yojson]

  let make () = ()
end

module Response = struct
  module Failure = struct
    type t = { error : string } [@@deriving yojson]
  end

  module Success = struct
    type t = { username : string } [@@deriving yojson]
  end
end

let send ~token ~ligo_registry ~request =
  let uri = Uri.with_path ligo_registry endpoint_uri in
  let headers =
    Cohttp.Header.of_list
      [ "referer", "cli-whoami"
      ; "Content-Type", "application/json"
      ; "Authorization", Printf.sprintf "Bearer %s" token
      ]
  in
  let* response, body = Lwt_result.ok @@ Cohttp_lwt_unix.Client.get ~headers uri in
  let* body = Lwt_result.ok @@ Cohttp_lwt.Body.to_string body in
  match Cohttp.Response.status response with
  | `OK ->
    let open Response.Success in
    let* { username } = body |> Yojson.Safe.from_string |> of_yojson |> Lwt_result.lift in
    (* TODO debug.log message *)
    Lwt_result.return @@ Some username
  | `Bad_gateway | `Service_unavailable | `Gateway_timeout ->
    (* TODO debug.log http code *)
    Lwt_result.lift @@ Error "\nRegistry seems down. Contact the developers"
  | `Unauthorized ->
    let open Response.Failure in
    let* { error : _ } =
      body |> Yojson.Safe.from_string |> of_yojson |> Lwt_result.lift
    in
    (* TODO debug.log message *)
    Lwt_result.return None
  | _ ->
    let open Response.Failure in
    let* { error : _ } =
      body |> Yojson.Safe.from_string |> of_yojson |> Lwt_result.lift
    in
    (* TODO debug.log message *)
    Lwt_result.return None


(* We purposely avoid the CLI interaction here and delegate the caller, Cli.ml, to handle the prompts and reset of user experience *)
let main ~token ~ligo_registry =
  let request = Request.make () in
  Lwt_main.run @@ send ~token ~request ~ligo_registry
