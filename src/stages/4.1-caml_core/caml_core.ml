open Ocaml_common
open Asttypes
open Ligo_prim
open Ast_core

(* TODO: explain, this id comes from OCaml  *)
type var_id = int

type type_ =
  { type_desc : type_desc
  ; type_loc : Location.t
  }

and type_desc =
  | T_var of (label option * var_id)
  | T_constr of Path.t * type_ list
  | T_arrow of type_ * type_
  | T_tuple of type_ list
  | T_forall of (label option * var_id) list * type_

type type_decl =
  { type_decl_desc : type_decl_desc
  ; type_decl_params : (label option * var_id) list
  ; type_decl_loc : Location.t
  }

and type_decl_desc =
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

(* TODO: improve Ligo such that this is not required *)
type var_pat =
  { var_pat_desc : Ident.t
  ; var_pat_type : type_
  ; var_pat_loc : Location.t
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
  | E_lambda of var_pat * expr
  | E_lambda_rec of
      { self : var_pat
      ; param : var_pat
      ; body : expr
      }
  | E_apply of expr * expr list
  | E_match of expr * (pat * expr) list
  | E_tuple of expr Ne_list.t
  | E_constructor of Label.t * expr list
  (* TODO: label on record? *)
  | E_record of (Label.t * expr) list
  | E_field of expr * Label.t

type mod_expr =
  { mod_expr_desc : mod_expr_desc
  ; mod_expr_loc : Location.t
  }

and mod_expr_desc =
  | M_var of Path.t
  | M_struct of decl list

(* TODO: rename declaration *)
and decl =
  { decl_desc : decl_desc
  ; decl_loc : Location.t
  }

and decl_desc =
  | D_let of (var_pat * expr)
  | D_type of (Ident.t * type_decl)
  | D_module of (Ident.t * mod_expr)
  | D_module_type of (Ident.t * sig_expr)
  (* FFI *)
  | D_external of Ident.t
  (* TODO: why arity here? *)
  | D_type_predef of (Ident.t * Literal_types.t * int)
  | D_type_unsupported of Ident.t

and sig_expr =
  { sig_expr_desc : sig_expr_desc
  ; sig_expr_loc : Location.t
  }

and sig_expr_desc =
  | S_var of Path.t
  | S_sig of sig_item list

and sig_item =
  { sig_item_desc : sig_item_desc
  ; sig_item_loc : Location.t
  }

and sig_item_desc =
  | S_value of (Ident.t * type_)
  | S_type of (Ident.t * type_decl)
  | S_module of (Ident.t * sig_item list)
  | S_module_type of (Ident.t * sig_item list)

type program = decl list

(* TODO: follow Ligo style *)
let type_wrap loc desc = { type_desc = desc; type_loc = loc }

let type_decl_wrap loc params desc =
  { type_decl_desc = desc; type_decl_params = params; type_decl_loc = loc }


let var_pat_wrap loc type_ desc =
  { var_pat_desc = desc; var_pat_type = type_; var_pat_loc = loc }


let pat_wrap loc type_ desc = { pat_desc = desc; pat_type = type_; pat_loc = loc }
let expr_wrap loc type_ desc = { expr_desc = desc; expr_type = type_; expr_loc = loc }
let mod_expr_wrap loc desc = { mod_expr_desc = desc; mod_expr_loc = loc }
let decl_wrap loc desc = { decl_desc = desc; decl_loc = loc }
let sig_expr_wrap loc desc = { sig_expr_desc = desc; sig_expr_loc = loc }
let sig_item_wrap loc desc = { sig_item_desc = desc; sig_item_loc = loc }
