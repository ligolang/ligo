type attributes = string list
  [@@deriving eq,compare,yojson,hash]

let pp_attributes ppf attributes =
  let attr =
    List.map ~f:(fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat
  in Format.fprintf ppf "%s" attr

type ('e, 't) t = {
    let_binder: 't Binder.t ;
    rhs       : 'e ;
    let_result: 'e ;
    attributes: attributes ;
  } [@@deriving eq,compare,yojson,hash]

let pp f g ppf = fun {let_binder; rhs; let_result; attributes=attr} ->
  Format.fprintf ppf "@[<v>let %a = %a%a in@,%a@]"
    (Binder.pp g) let_binder
    f rhs
    pp_attributes attr
    f let_result

let fold : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) t -> 'acc
= fun f g acc { let_binder; rhs ; let_result; attributes=_} ->
  let acc = Binder.fold g acc let_binder in
  let acc = f acc rhs in
  let acc = f acc let_result in
   acc

let map :  ('a -> 'b) -> ('c -> 'd) -> ('a,'c) t -> ('b,'d) t
= fun f g {let_binder; rhs; let_result; attributes} ->
  let let_binder = Binder.map g let_binder in
  let rhs        = f rhs in
  let let_result = f let_result in
  {let_binder; rhs; let_result; attributes}

let fold_map :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
= fun f g acc {let_binder; rhs; let_result; attributes} ->
  let acc,let_binder = Binder.fold_map g acc let_binder in
  let acc,rhs        = f acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{let_binder; rhs; let_result; attributes})
