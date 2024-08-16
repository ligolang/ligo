open Ocaml_common
open Asttypes
open Ligo_prim
open Ast_core

(* TODO: explain, this id comes from OCaml  *)
type var_id = int

(* TODO: location on types *)
type type_ =
  | T_var of (label option * var_id)
  | T_constr of Path.t * type_ list
  | T_arrow of type_ * type_
  | T_tuple of type_ list
  | T_forall of (label option * var_id) list * type_

type type_decl =
  | T_record of type_decl_label list
  | T_variant of type_decl_case list
  | T_alias of type_

and type_decl_label =
  { dl_id : Ident.t
  ; dl_type : type_
  ; dl_loc : Location.t
  }

and type_decl_case =
  | C_tuple of
      { dc_id : Ident.t
      ; dc_fields : type_ list
      ; dc_loc : Location.t
      }
  | C_record of
      { dc_id : Ident.t
      ; dc_fields : type_decl_label list
      ; dc_loc : Location.t
      }

type pat =
  { pat_desc : pat_desc
  ; pat_type : type_
  ; pat_loc : Location.t
  }

and pat_desc =
  | P_unit
  | P_var of Ident.t
  | P_tuple of pat list
  | P_record of (Label.t * pat) list
  | P_variant of (Label.t * pat)

(* TODO: drop var_pat? *)
type var_pat =
  { var_pat_desc : Ident.t
  ; var_pat_type : type_
  ; var_pat_loc : Location.t
  }

type expr =
  { expr_desc : expr_desc
  ; expr_type : type_
  ; expr_loc : Location.t
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
  | E_constructor of Label.t * expr list
  (* TODO: label on record? *)
  | E_record of (label * expr) list
  | E_field of expr * label

type mod_expr =
  { mod_expr_desc : mod_expr_desc
  ; mod_expr_loc : Location.t
  }

and mod_expr_desc =
  | M_var of Path.t
  | M_struct of decl list

and decl =
  { decl_desc : decl_desc
  ; decl_loc : Location.t
  }

and decl_desc =
  | D_value of (var_pat * expr)
  (* TODO: type record and type abstract *)
  | D_type of (Ident.t * type_decl)
  | D_module of (Ident.t * mod_expr)
