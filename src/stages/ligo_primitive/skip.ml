type t = unit
  [@@deriving eq,compare,yojson,hash,fold,map]

let pp ppf = fun () ->
  Format.fprintf ppf "skip"


let fold_map : 'acc -> unit -> 'acc * unit
= fun acc () ->
  (acc,())
