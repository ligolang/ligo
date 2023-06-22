module Location = Simple_utils.Location
module PP = Simple_utils.PP_helpers
open Ligo_prim

module Attr = struct
  type t =
    { entry : bool
    ; view : bool
    }
  [@@deriving compare, hash, equal]

  let default = { entry = false; view = false }
  let of_core_attr ({ entry; view; _ } : Ast_core.ValueAttr.t) = { entry; view }
end

type t =
  { tvars : Type_var.t list
  ; items : item list
  }

and item =
  | MT_value of Value_var.t * Type.t * Attr.t
  | MT_type of Type_var.t * Type.t
[@@deriving compare, hash, equal]

let list ~pp ppf xs =
  let rec loop ppf = function
    | [] -> Format.fprintf ppf ""
    | x :: xs -> Format.fprintf ppf "%a@.%a" pp x loop xs
  in
  Format.fprintf ppf "@[<v>%a@]" loop xs


let rec pp_item ppf item =
  match item with
  | MT_value (var, type_, _attr) ->
    Format.fprintf ppf "val %a : %a" Value_var.pp var Type.pp type_
  | MT_type (tvar, type_) ->
    Format.fprintf ppf "type %a = %a" Type_var.pp tvar Type.pp type_


and pp_tvar ppf tvar = Format.fprintf ppf "type %a" Type_var.pp tvar

and pp ppf { tvars; items } =
  Format.fprintf
    ppf
    "@[<v>sig@,%a%a@,end@]"
    (list ~pp:pp_tvar)
    tvars
    (list ~pp:pp_item)
    items


let rec instantiate_var mt ~tvar ~type_ =
  let self mt = instantiate_var mt ~tvar ~type_ in
  match mt with
  | [] -> mt
  | MT_value (val_var, val_type, val_attr) :: mt ->
    let val_type = Type.subst val_type ~tvar ~type_ in
    MT_value (val_var, val_type, val_attr) :: self mt
  | MT_type (type_var, type_type) :: mt ->
    let type_type = Type.subst type_type ~tvar ~type_ in
    MT_type (type_var, type_type) :: self mt
