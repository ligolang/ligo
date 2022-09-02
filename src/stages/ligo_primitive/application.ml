type 'a t = {
  lamb: 'a ;
  args: 'a ;
  } [@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {lamb;args} ->
  Format.fprintf ppf "@[<hv>(%a)@@(%a)@]"
    f lamb
    f args

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {lamb;args} ->
  let acc = f acc lamb in
  let acc = f acc args in
   acc

let map : ('a -> 'b) -> 'a t -> 'b t
= fun f {lamb;args} ->
  let lamb = f lamb in
  let args = f args in
  {lamb; args}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {lamb;args} ->
  let acc,lamb = f acc lamb in
  let acc,args = f acc args in
  (acc,{lamb; args})
