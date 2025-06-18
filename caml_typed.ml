type arg_label
type var
type kind
type id = int

type type_ =
  (* meta *)
  | T_loc of Location.t * type_
  | T_shared of id * type_
  | T_alias of id
  (* TODO: label name *)
  | T_arrow of type_ * type_
  | T_var of var
  | T_forall of var * type_
  | T_apply of type_ * type_
  (* ADTs *)
  | T_never
  | T_sum of row * type_
  (* Records *)
  | T_empty
  | T_record of row * type_
  (* Tuples *)
  | T_unit
  | T_pair of type_ * type_

(* TODO: type level let sharing? *)
(* TODO: named tuples *)
and row

open Types
open Typedtree

let extract_tuple fields f =
  let init = T_unit in
  List.fold_right fields ~init ~f:(fun left right ->
      let left = f left in
      T_pair (left, right))

let extract_constr path args f = _

let rec extract_type_expr tbl typ =
  let id = get_id typ in
  match Hashtbl.mem tbl id with
  | true -> T_alias id
  | false ->
    let typ = extract_type_expr_desc tbl typ in
    T_shared (id, typ)

and extract_type_expr_desc tbl typ =
  let extract_type_expr typ = extract_type_expr tbl typ in
  match get_desc typ with
  | Tvar _ -> _
  (* TODO: _comm? *)
  | Tarrow (Nolabel, param, body, _comm) ->
    let param = extract_type_expr param in
    let body = extract_type_expr body in
    T_arrow (param, body)
  | Tarrow ((Labelled _ | Optional _), param, body, _) -> _
  | Ttuple fields -> extract_tuple fields extract_type_expr
  (* TODO: _abbrev? *)
  | Tconstr (path, args, _abbrev) -> extract_constr path args extract_type_expr
  | Tobject (_, _) -> _
  | Tfield (_, _, _, _) -> _
  | Tnil -> _
  | Tlink _ -> _
  | Tsubst (_, _) -> _
  | Tvariant _ -> _
  | Tunivar _ -> _
  | Tpoly (_, _) -> _
  | Tpackage (_, _) -> _

let rec extract_type_declaration typ_decl =
  let { type_params
      ; type_arity
      ; type_kind
      ; type_private
      ; type_manifest
      ; type_variance
      ; type_separability
      ; type_is_newtype
      ; type_expansion_scope
      ; type_loc = loc
      ; type_attributes
      ; type_immediate
      ; type_unboxed_default
      ; type_uid
      }
    =
    typ_decl
  in
  match type_kind with
  | Type_abstract -> _
  | Type_record (rows, repr) -> _
  | Type_variant (rows, repr) -> _
  | Type_open -> _

let rec extract_core_type ctyp =
  (* TODO: this is only used because Types.type_expr lacks a location *)
  let { ctyp_desc; ctyp_type; ctyp_env; ctyp_loc = loc; ctyp_attributes } = ctyp in
  match ctyp_desc with
  | Ttyp_any -> _
  | Ttyp_var _ -> _
  | Ttyp_arrow (Nolabel, param, body) ->
    let param = extract_core_type param in
    let body = extract_core_type body in
    type_wrap loc @@ T_arrow (param, body)
  | Ttyp_arrow ((Labelled _ | Optional _), _, _) -> _
  | Ttyp_tuple fields -> extract_tuple ~loc fields extract_core_type
  | Ttyp_constr (_, _, _) -> _
  | Ttyp_object (_, _) -> _
  | Ttyp_class (_, _, _) -> _
  | Ttyp_alias (_, _) ->
    (* TODO: names? *)
    _
  | Ttyp_variant (_, _, _) -> _
  | Ttyp_poly (_, _) -> _
  | Ttyp_package _ -> _
