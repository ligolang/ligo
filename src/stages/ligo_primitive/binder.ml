module Option = Simple_utils.Option

type 'a t = {
  var  : Var.Value_var.t ;
  ascr : 'a ;
  } [@@deriving eq,compare,yojson,hash,fold,map,iter]

let make var ascr = {
  var ; ascr 
}

let set_var (b : _ t) (var : Var.Value_var.t) = {b with var}
let get_var (b : 'a t) = b.var
let get_ascr (b : 'a t) = b.ascr
let set_ascr b ascr = { b with ascr }
let get_loc (b: 'a t) = Var.Value_var.get_location b.var

let apply f (b : 'a t) = f b.var

let equal_var (a : 'a t) (b : _ t) = Var.Value_var.equal a.var b.var
let compare_var (a : 'a t) (b : _ t) = Var.Value_var.compare a.var b.var


let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {var; ascr} ->
  let acc,ascr = f acc ascr in
  (acc,{var; ascr})

let pp g ppf {var;ascr} =
  Format.fprintf ppf "%a%a"
    Var.Value_var.pp var
    g ascr

