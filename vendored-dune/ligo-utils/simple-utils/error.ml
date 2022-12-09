type content =
  { message : string
  ; location : Location.t option
  ; children : t option
  }

and t =
  { status : string
  ; stage : string
  ; content : content
  }

let make_content ~message ?location ?children () =
  { message; location; children }

let status = "error"
let make ~stage ~content = { status; stage; content }

let rec to_yojson : t -> Yojson.Safe.t 
  = fun e ->
    let open Yojson.Safe in
    let { status ; stage ; content } = e in
    let { message ; location ; children } = content in
    let location = 
      Option.map ~f:Location.to_yojson location 
      |> Option.map ~f:(fun x -> ["location", x]) 
      |> Option.value ~default:[] 
    in
    let children = 
      Option.map ~f:to_yojson children 
      |> Option.map ~f:(fun x -> ["children", x]) 
      |> Option.value ~default:[] 
    in
    let content =
      `Assoc
        ([ ("message", `String message) ] @ location @ children)
    in
    `Assoc 
      [ ("status", `String status)
      ; ("stage", `String stage)
      ; ("content", content)
      ]