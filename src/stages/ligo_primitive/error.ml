open Simple_utils

type content =
  { message : string
  ; location : Location.t option
  ; children : t option
  }
[@@deriving yojson]

and t =
  { status : string
  ; stage : string
  ; content : content
  }

let make_content ~message ?location ?children () =
  { message; location; children }


let status = "error"
let make ~stage ~content = { status; stage; content }
