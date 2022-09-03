module Option = Simple_utils.Option
type binder_attributes = {
    const_or_var : [`Const | `Var] option;
  } [@@deriving eq,compare,yojson,hash]

let const_attribute = {
    const_or_var = Some `Const
  }

let var_attribute = {
    const_or_var = Some `Var
  }
let empty_attribute = {
    const_or_var = None
  }
type 'a t = {
  var  : Var.ValueVar.t ;
  ascr : 'a ;
  attributes : binder_attributes ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp g ppf {var;ascr;attributes={const_or_var}} =
  let open Format in
  let option_const_or_var ppf is_var =
    match is_var with
    | None -> fprintf ppf ""
    | Some `Var -> fprintf ppf "[@var]"
    | Some `Const -> fprintf ppf ""
  in
  Format.fprintf ppf "%a%a%a"
    Var.ValueVar.pp var
    option_const_or_var const_or_var
    g ascr

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {var; ascr; attributes} ->
  let acc,ascr = f acc ascr in
  (acc,{var; ascr; attributes})

