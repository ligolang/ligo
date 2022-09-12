module Option = Simple_utils.Option
type binder_attributes = {
    mut : bool; (* Mutability*)
  } [@@deriving eq,compare,yojson,hash]

type 'a t = {
  var  : Var.Value_var.t ;
  ascr : 'a ;
  attributes : binder_attributes ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let make ?(mut=false) var ascr = {
  var ; ascr ; attributes = {mut}
}

let set_var (b : _ t) (var : Var.Value_var.t) = {b with var}
let get_var (b : 'a t) = b.var
let get_ascr (b : 'a t) = b.ascr

let apply f (b : 'a t) = f b.var

let equal_var (a : 'a t) (b : _ t) = Var.Value_var.equal a.var b.var

let is_mutable (b : _ t) = b.attributes.mut
let make_const (b : _ t) =  {b with attributes={mut=false}}


let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {var; ascr; attributes} ->
  let acc,ascr = f acc ascr in
  (acc,{var; ascr; attributes})

let pp g ppf {var;ascr;attributes={mut}} =
  let open Format in
  let option_const_or_var ppf is_var =
    if is_var then
      fprintf ppf "[@var]"
    else fprintf ppf ""
  in
  Format.fprintf ppf "%a%a%a"
    Var.Value_var.pp var
    option_const_or_var mut
    g ascr

