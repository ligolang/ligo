type 'a t = {
  ty_binder : Var.TypeVar.t;
  kind : Kind.t;
  type_ : 'a ;
} [@@deriving eq,compare,yojson,hash]
(* Lambda (a : kind). term *)


let pp f ppf ({ty_binder ; kind ; type_}) : unit =
  Format.fprintf ppf "funtype %a : %a . %a"
    Var.TypeVar.pp ty_binder
    Kind.pp kind
    f type_

let map : ('a -> 'b) -> 'a t -> 'b t =
  fun f ({ type_ ; _} as fa) ->
    let type_ = f type_ in
    { fa with type_}
