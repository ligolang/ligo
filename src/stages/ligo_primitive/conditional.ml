type 'e t = {
  condition   : 'e ;
  then_clause : 'e ;
  else_clause : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {condition; then_clause; else_clause} ->
  Format.fprintf ppf "if %a then %a else %a"
    f condition
    f then_clause
    f else_clause

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {condition;then_clause;else_clause} ->
  let acc = f acc condition in
  let acc = f acc then_clause in
  let acc = f acc else_clause in
  acc

let map : ('a -> 'b) -> 'a t -> 'b t
= fun f {condition;then_clause;else_clause} ->
  let condition   = f condition in
  let then_clause = f then_clause in
  let else_clause = f else_clause in
  {condition;then_clause;else_clause}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {condition;then_clause;else_clause} ->
  let acc,condition   = f acc condition in
  let acc,then_clause = f acc then_clause in
  let acc,else_clause = f acc else_clause in
  (acc,{condition;then_clause;else_clause})
