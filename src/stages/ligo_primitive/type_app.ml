type 'ty_exp t = {
  type_operator : Var.TypeVar.t ;
  arguments     : 'ty_exp list ;
} [@@deriving eq,compare,yojson,hash]

let pp g ppf ({type_operator ; arguments}: 'a t) : unit =
  Format.fprintf ppf "%a%a"
    Var.TypeVar.pp type_operator
    Simple_utils.PP_helpers.(list_sep_d_par g) arguments


let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun g acc {type_operator=_;arguments} ->
  let acc = List.fold ~f:g ~init:acc arguments in
   acc

let map : ('a -> 'b) -> 'a t -> 'b t
= fun g {type_operator;arguments} ->
  let arguments = List.map ~f:g arguments in
  {type_operator; arguments}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc *  'a t
= fun g acc {type_operator;arguments} ->
  let acc,arguments = List.fold_map ~f:g ~init:acc arguments in
  (acc,{type_operator; arguments})
