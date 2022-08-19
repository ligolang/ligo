type t = unit
  [@@deriving eq,compare,yojson,hash]

let pp ppf = fun () ->
  Format.fprintf ppf "skip"


let fold : 'acc -> unit -> 'acc
= fun acc () ->
  acc

let map : unit -> unit
= fun () ->
  ()

let fold_map : 'acc -> unit -> 'acc * unit
= fun acc () ->
  (acc,())
