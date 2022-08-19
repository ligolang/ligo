type 'e t = {
  language : string ;
  code : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {language; code} ->
  Format.fprintf ppf "[%%%s %a]"
    language
    f code

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {language=_;code} ->
  let acc = f acc code in
  acc

let map : ('a -> 'b) -> 'a t -> 'b t
= fun f {language;code} ->
  let code = f code in
  {language;code}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {language;code} ->
  let acc,code = f acc code in
  (acc,{language;code})
