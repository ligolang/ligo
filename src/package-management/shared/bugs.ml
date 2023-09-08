(* This module is shared between Ligo_manifest and Registry_response. *)

type t =
  { url : string
  ; email : string option
  }

let of_yojson json =
  let open Yojson.Safe in
  try
    let url = Util.to_string @@ Util.member "url" json in
    let email = Util.to_string_option @@ Util.member "email" json in
    Ok { url; email }
  with
  | _ -> Error "Failed to parse url and email from json"


let to_yojson { url; email } =
  `Assoc
    ([ "url", `String url ]
    @ Option.value_map email ~default:[] ~f:(fun email -> [ "email", `String email ]))

(* If required validate `email` & url` *)
