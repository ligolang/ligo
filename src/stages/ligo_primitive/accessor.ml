module type Access = sig
  type 'a t
  [@@deriving eq,compare,yojson,hash]
  val pp   : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val map  : ('a -> 'b) -> 'a t -> 'b t
  val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
end
module Make(Access : Access) = struct
  type 'e t = {
    record: 'e;
    path: 'e Access.t;
    } [@@deriving eq,compare,yojson,hash]

  let pp f ppf = fun ({record;path}: _ t) ->
    Format.fprintf ppf "%a.%a"
      f record
      (Access.pp f) path

  let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  = fun f acc {record;path=p} ->
    let acc = f acc record in
    let acc = Access.fold f acc p in
    acc

  let map : ('a -> 'b) -> 'a t -> 'b t
  = fun f {record;path=p} ->
    let record = f record in
    let path   = Access.map f p in
    {record;path}

  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
  = fun f acc {record;path=p} ->
    let acc,record = f acc record in
    let acc,path   = Access.fold_map f acc p in
    (acc,{record;path})

end
