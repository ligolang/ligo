open Ocaml_common
open Asttypes
open Types
open Ligo_prim
open Ast_core

(* TODO: explain, this id comes from OCaml  *)
type var_id = int

type type_ =
  | T_var of (label option * var_id)
  | T_constr of Path.t * type_ list
  | T_arrow of type_ * type_
  | T_tuple of type_ list
  | T_forall of (label * var_id) list * type_

type type_decl =
  | T_record of (Ident.t * type_) list
  | T_alias of type_

type pat =
  { pat_desc : pat_desc
  ; pat_type : type_
  }

and pat_desc =
  | P_unit
  | P_var of Ident.t
  | P_tuple of pat list
  | P_record of (label_description * pat) list
  | P_variant of (constructor_description * pat)

(* TODO: drop var_pat? *)
type var_pat =
  { var_pat_desc : Ident.t
  ; var_pat_type : type_
  }

type expr =
  { expr_desc : expr_desc
  ; expr_type : type_
  }

and expr_desc =
  | E_var of Path.t
  | E_literal of Literal_value.t
  (* TODO: tag poly expressions and patterns here? *)
  | E_let of pat * expr * expr
  | E_let_rec of var_pat * expr * expr
  | E_lambda of var_pat * expr
  | E_apply of expr * expr list
  | E_match of expr * (pat * expr) list
  | E_tuple of expr Ne_list.t
  (* TODO: label on record? *)
  | E_record of (label * expr) list
  | E_field of expr * label

type decl = { decl_desc : decl_desc }

and decl_desc =
  | D_value of (var_pat * expr)
  (* TODO: type record and type abstract *)
  | D_type of (Ident.t * type_decl)
