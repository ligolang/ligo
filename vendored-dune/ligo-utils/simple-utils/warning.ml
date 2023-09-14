type content =
  { message : string
  ; location : Location.t
  ; variable : string option
  }

type t =
  { status : string
  ; stage : string
  ; content : content
  }

let make_content ~message ~location ?variable () = { message; location; variable }
let status = "warning"
let make ~stage ~content = { status; stage; content }

let rec to_yojson : t -> Yojson.Safe.t =
 fun e ->
  let open Yojson.Safe in
  let { status; stage; content } = e in
  let { message; location; variable } = content in
  let variable =
    Option.map ~f:(fun s -> `String s) variable
    |> Option.map ~f:(fun x -> [ "variable", x ])
    |> Option.value ~default:[]
  in
  let location = [ "location", Location.to_yojson location ] in
  let content = `Assoc ([ "message", `String message ] @ location @ variable) in
  `Assoc [ "status", `String status; "stage", `String stage; "content", content ]
