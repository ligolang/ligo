type 'ty_exp t = {
  type_operator : Var.TypeVar.t ;
  arguments     : 'ty_exp list ;
} [@@deriving eq,compare,yojson,hash,fold,map]

let pp g ppf ({type_operator ; arguments}: 'a t) : unit =
  Format.fprintf ppf "%a%a"
    Var.TypeVar.pp type_operator
    Simple_utils.PP_helpers.(list_sep_d_par g) arguments


let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc *  'a t
= fun g acc {type_operator;arguments} ->
  let acc,arguments = List.fold_map ~f:g ~init:acc arguments in
  (acc,{type_operator; arguments})
