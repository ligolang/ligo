type 'a t = {
  ty_binder : Var.Type_var.t;
  kind : Kind.t;
  type_ : 'a ;
} [@@deriving eq,compare,yojson,hash,fold,map]
(* Lambda (a : kind). term *)


let pp f ppf ({ty_binder ; kind ; type_}) : unit =
  Format.fprintf ppf "funtype %a : %a . %a"
    Var.Type_var.pp ty_binder
    Kind.pp kind
    f type_
