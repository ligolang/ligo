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
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f g ppf = fun {let_binder; rhs; let_result; attributes=attr} ->
  Format.fprintf ppf "@[<v>let %a = %a%a in@,%a@]"
    (Binder.pp g) let_binder
    f rhs
    pp_attributes attr
    f let_result

let pp_mut f g ppf = fun {let_binder; rhs; let_result; attributes=attr} ->
  Format.fprintf ppf "@[<v>let mut %a = %a%a in@,%a@]"
    (Binder.pp g) let_binder
    f rhs
    pp_attributes attr
    f let_result

let fold_map :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
= fun f g acc {let_binder; rhs; let_result; attributes} ->
  let acc,let_binder = Binder.fold_map g acc let_binder in
  let acc,rhs        = f acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{let_binder; rhs; let_result; attributes})
