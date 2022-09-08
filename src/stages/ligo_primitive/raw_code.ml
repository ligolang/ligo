type 'e t = {
  language : string ;
  code : 'e ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f ppf = fun {language; code} ->
  Format.fprintf ppf "[%%%s %a]"
    language
    f code

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {language;code} ->
  let acc,code = f acc code in
  (acc,{language;code})
