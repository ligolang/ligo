type 'e t = {
  condition   : 'e ;
  then_clause : 'e ;
  else_clause : 'e ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f ppf = fun {condition; then_clause; else_clause} ->
  Format.fprintf ppf "if %a then %a else %a"
    f condition
    f then_clause
    f else_clause

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {condition;then_clause;else_clause} ->
  let acc,condition   = f acc condition in
  let acc,then_clause = f acc then_clause in
  let acc,else_clause = f acc else_clause in
  (acc,{condition;then_clause;else_clause})
