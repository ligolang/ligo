open Ligo_prim
open Ast_core
open Caml_error

(* TODO: explain, this id comes from OCaml  *)

type type_ =
  { type_desc : type_desc
  ; type_loc : Location.t
  }

and type_desc =
  | T_var of Type_var.t
  | T_constr of Type_var.t Module_access.t * type_ list
  | T_arrow of type_ * type_
  | T_tuple of type_ list
  | T_forall of Type_var.t * type_
  | T_error of error

type type_decl =
  { type_decl_desc : type_decl_desc
  ; type_decl_params : Type_var.t list
  ; type_decl_loc : Location.t
  }

and type_decl_desc =
  | T_record of type_ Label.Map.t
  | T_variant of type_decl_case Label.Map.t
  | T_alias of type_
  | T_error of error

and type_decl_case =
  | C_tuple of
      { dc_id : Label.t
      ; dc_fields : type_ list
      ; dc_loc : Location.t
      }
  | C_record of
      { dc_id : Label.t
      ; dc_fields : type_ Label.Map.t
      ; dc_loc : Location.t
      }

type pat =
  { pat_desc : pat_desc
  ; pat_type : type_
  ; pat_loc : Location.t
  }

and pat_desc =
  | P_unit
  | P_var of Value_var.t
  | P_tuple of pat list
  | P_record of pat Label.Map.t
  | P_variant of (Label.t * pat)
  | P_error of error

(* TODO: error *)
type var_pat =
  { var_pat_desc : var_pat_desc
  ; var_pat_type : type_
  ; var_pat_loc : Location.t
  }

and var_pat_desc =
  | VP_var of Value_var.t
  | VP_error of error

type expr =
  { expr_desc : expr_desc
  ; expr_type : type_
  ; expr_loc : Location.t
  }

and expr_desc =
  | E_var of Value_var.t Module_access.t
  | E_literal of Literal_value.t
  | E_constant of expr Constant.t
  (* TODO: tag poly expressions and patterns here? *)
  | E_let of
      { binder : pat
      ; foralls : Type_var.t list
      ; attr : Value_attr.t
      ; value : expr
      ; body : expr
      }
  | E_let_module of Module_var.t * mod_expr * expr
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
  | E_record of expr Label.Map.t
  | E_field of expr * Label.t
  | E_error of error

and mod_expr =
  { mod_expr_desc : mod_expr_desc
  ; mod_expr_loc : Location.t
  }

and mod_expr_desc =
  | M_var of Module_var.t Module_access.t
  | M_struct of decl list

(* TODO: rename declaration *)
and decl =
  { decl_desc : decl_desc
  ; decl_loc : Location.t
  }

and decl_desc =
  | D_let of
      { binder : var_pat
      ; foralls : Type_var.t list
      ; attr : Value_attr.t
      ; value : expr
      }
  | D_type of (Type_var.t * Type_or_module_attr.t * type_decl)
  | D_type_predef of (Type_var.t * Literal_types.t * int)
  | D_module of (Module_var.t * Type_or_module_attr.t * mod_expr)
  | D_module_type of (Module_var.t * Signature_attr.t * sig_expr)
  | D_error of error

and sig_expr =
  { sig_expr_desc : sig_expr_desc
  ; sig_expr_loc : Location.t
  }

and sig_expr_desc =
  | S_var of Module_var.t Module_access.t
  | S_sig of sig_item list

and sig_item =
  { sig_item_desc : sig_item_desc
  ; sig_item_loc : Location.t
  }

and sig_item_desc =
  | S_value of (Value_var.t * Sig_item_attr.t * type_)
  | S_type of (Type_var.t * SigTypeAttr.t * type_decl)
  | S_module of (Module_var.t * sig_item list)
  | S_module_type of (Module_var.t * sig_item list)
  | S_error of error

type program = decl list

let type_wrap loc desc : type_ = { type_desc = desc; type_loc = loc }

let type_decl_wrap loc params desc : type_decl =
  { type_decl_desc = desc; type_decl_params = params; type_decl_loc = loc }


let pat_wrap loc type_ desc : pat = { pat_desc = desc; pat_type = type_; pat_loc = loc }

let var_pat_wrap loc type_ desc : var_pat =
  { var_pat_desc = desc; var_pat_type = type_; var_pat_loc = loc }


let expr_wrap loc type_ desc : expr =
  { expr_desc = desc; expr_type = type_; expr_loc = loc }


let mod_expr_wrap loc desc : mod_expr = { mod_expr_desc = desc; mod_expr_loc = loc }
let decl_wrap loc desc : decl = { decl_desc = desc; decl_loc = loc }
let sig_expr_wrap loc desc : sig_expr = { sig_expr_desc = desc; sig_expr_loc = loc }
let sig_item_wrap loc desc : sig_item = { sig_item_desc = desc; sig_item_loc = loc }
