type 'e t = {
  constructor: Label.t;
  element: 'e;
  } [@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {constructor;element} ->
  Format.fprintf ppf "@[%a(%a)@]"
    Label.pp constructor
    f element

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {constructor=_;element} ->
  let acc = f acc element in
   acc

let map : ('a -> 'b) -> 'a t -> 'b t
= fun f {constructor;element} ->
  let element = f element in
  {constructor; element}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {constructor;element} ->
  let acc,element = f acc element in
  (acc,{constructor; element})
