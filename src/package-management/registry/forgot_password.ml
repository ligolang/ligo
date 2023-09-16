open Lwt_result.Syntax

let endpoint_uri = "/-/cli/forgot-password"

module Request = struct
  type t = { userName : string } [@@deriving yojson]

  let make ~userName = { userName }
end

module Response = struct
  module Failure = struct
    type code =
      | No_profile
      | Email_not_verified

    let code_of_yojson = function
      | `Int 1 -> Ok No_profile
      | `Int 2 -> Ok Email_not_verified
      | y -> Error ("Expected code 1 or 2. Got " ^ Yojson.Safe.to_string y)


    let code_to_yojson = function
      | No_profile -> `Int 1
      | Email_not_verified -> `Int 2


    let code_to_error_msg = function
      | No_profile -> "We now have profiles, and it seems you haven't created one."
      | Email_not_verified -> "It seems you haven't verified your email"


    type t =
      { code : code
      ; message : string
      }
    [@@deriving yojson]
  end

  module Success = struct
    type t = unit [@@deriving yojson]
  end
end

type state =
  | Password_reset_emailed
  | Profile_creation_pending
  | Email_verification_pending

(* Forgot password endpoint must never ask for token *)
let send ~ligo_registry ~request =
  let uri = Uri.with_path ligo_registry endpoint_uri in
  let body =
    request |> Request.to_yojson |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
  in
  let headers =
    Cohttp.Header.of_list
      [ "referer", "cli-forgot-password"; "Content-Type", "application/json" ]
  in
  let* response, body = Lwt_result.ok @@ Cohttp_lwt_unix.Client.post ~headers ~body uri in
  let* body = Lwt_result.ok @@ Cohttp_lwt.Body.to_string body in
  match Cohttp.Response.status response with
  | `OK -> Lwt_result.return Password_reset_emailed
  | `Bad_gateway | `Service_unavailable | `Gateway_timeout ->
    (* TODO debug.log http code *)
    Lwt_result.lift @@ Error "\nRegistry seems down. Contact the developers"
  | `Not_implemented ->
    let open Response.Failure in
    let* { code; message : _ } =
      body |> Yojson.Safe.from_string |> of_yojson |> Lwt_result.lift
    in
    (* TODO debug.log message *)
    let state =
      match code with
      | No_profile -> Profile_creation_pending
      | Email_not_verified -> Email_verification_pending
    in
    Lwt_result.return state
  | _ ->
    let open Response.Failure in
    let* { code; message : _ } =
      body |> Yojson.Safe.from_string |> of_yojson |> Lwt_result.lift
    in
    (* TODO debug.log message *)
    let msg = Response.Failure.code_to_error_msg code in
    Lwt_result.lift @@ Error msg


(* We purposely avoid the CLI interaction here and delegate the caller, Cli.ml, to handle the prompts and reset of user experience *)
let main ~username ~ligo_registry ~ligorc_path =
  let request = Request.make ~userName:username in
  Lwt_main.run @@ send ~request ~ligo_registry
