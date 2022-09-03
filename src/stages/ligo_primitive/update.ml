module type Access = sig
  type 'a t
  [@@deriving eq,compare,yojson,hash,fold,map]
  val pp   : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
end
module Make(Access : Access) = struct
  type 'e t = {
    record: 'e ;
    path: 'e Access.t ;
    update: 'e ;
    } [@@deriving eq,compare,yojson,hash,fold,map]

  let pp f ppf = fun ({record; path; update}: _ t) ->
    Format.fprintf ppf "{ %a with %a = %a }"
      f record
      (Access.pp f) path
      f update

    let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
  = fun f acc {record;path=p;update} ->
    let acc,record = f acc record in
    let acc,path   = Access.fold_map f acc p in
    let acc,update = f acc update in
    (acc,{record;path;update})

end
