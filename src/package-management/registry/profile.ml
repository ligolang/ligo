open Lwt_result.Syntax

let endpoint_uri = "/-/api/-/npm/v1/user"

module Request = struct
  type t =
    { email : string
    ; fullname : string
    }
  [@@deriving yojson]

  let make ~email ~fullname = { email; fullname }
end

module Response = struct
  module Success = struct
    type t = unit [@@deriving yojson]
  end

  module Failure = struct
    type t = string [@@deriving yojson]
  end
end

type error =
  | Server_error
  | Invalid_token
  | Invalid_input

let send ~token ~ligo_registry ~request =
  let uri = Uri.with_path ligo_registry endpoint_uri in
  let body =
    request |> Request.to_yojson |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
  in
  let headers =
    Cohttp.Header.of_list
      [ "referer", "cli-forgot-password"
      ; "Content-Type", "application/json"
      ; "Authorization", Printf.sprintf "Bearer %s" token
      ]
  in
  let* response, body = Lwt_result.ok @@ Cohttp_lwt_unix.Client.post ~headers ~body uri in
  let* body = Lwt_result.ok @@ Cohttp_lwt.Body.to_string body in
  match Cohttp.Response.status response with
  | `OK -> Lwt_result.return ()
  | `Unauthorized -> Lwt_result.lift @@ Error Invalid_token
  | _ ->
    (* TODO debug.log _ *)
    (* At the moment, server doesn't throw any validation errors. Any error, for now, is assumed to be Internal server error *)
    Lwt_result.lift @@ Error Server_error


let create ~ligo_registry ~token ~email ~fullname =
  let request = Request.make ~email ~fullname in
  send ~token ~request ~ligo_registry
