(* The contents of this file should be auto-generated. *)

open Ast_typed.Types
module T = Ast_typed.Types

(* TODO: look for a ppx to create comparison from an ordering *)
let compare_simple_c_constant = function
  | C_arrow -> (function
      (* N/A -> 1 *)
      | C_arrow -> 0
      | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_option -> (function
      | C_arrow -> 1
      | C_option -> 0
      | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_map -> (function
      | C_arrow | C_option  -> 1
      | C_map -> 0
      | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_big_map -> (function
      | C_arrow | C_option  | C_map -> 1
      | C_big_map -> 0
      | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_list -> (function
      | C_arrow | C_option  | C_map | C_big_map -> 1
      | C_list -> 0
      | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_set -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list -> 1
      | C_set -> 0
      | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_unit -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set -> 1
      | C_unit -> 0
       | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_string -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  -> 1
      | C_string -> 0
      | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_nat -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string -> 1
      | C_nat -> 0
      | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_mutez -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat -> 1
      | C_mutez -> 0
      | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_timestamp -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez -> 1
      | C_timestamp -> 0
      | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_int -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp -> 1
      | C_int -> 0
      | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_address -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int -> 1
      | C_address -> 0
      | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_bytes -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address -> 1
      | C_bytes -> 0
      | C_key_hash | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_key_hash -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes -> 1
      | C_key_hash -> 0
      | C_key | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_key -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash -> 1
      | C_key -> 0
      | C_signature | C_operation | C_contract | C_chain_id -> -1)
  | C_signature -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key -> 1
      | C_signature -> 0
      | C_operation | C_contract | C_chain_id -> -1)
  | C_operation -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature -> 1
      | C_operation -> 0
      | C_contract | C_chain_id -> -1)
  | C_contract -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation -> 1
      | C_contract -> 0
      | C_chain_id -> -1)
  | C_chain_id -> (function
      | C_arrow | C_option | C_map | C_big_map | C_list | C_set | C_unit  | C_string | C_nat | C_mutez | C_timestamp | C_int | C_address | C_bytes | C_key_hash | C_key | C_signature | C_operation | C_contract -> 1
      | C_chain_id -> 0
      (* N/A -> -1 *)
    )

let compare_simple_c_row = function
  | C_record  -> (function 
    (* N/A -> 1 *)
    | C_record  -> 0
    | C_variant -> -1)
  | C_variant -> (function
    | C_record  -> 1
    | C_variant -> 0
    (* N/A -> -1 *))

(* cb is a function to allow lazy evaluation *)
let (<?) ca cb =
  if ca = 0 then cb () else ca
let compare_type_variable a b =
  Var.compare a b
let compare_label (a:label) (b:label) = 
  let Label a = a in
  let Label b = b in
  String.compare a b
let compare_lmap f ma mb =
  let la = LMap.to_kv_list ma in
  let lb = LMap.to_kv_list mb in
  let f  = fun (xa,ya) (xb, yb) ->
    compare_label xa xb <? fun () -> f ya yb 
  in
  List.compare ~compare:f la lb

let rec compare_typeclass a b = List.compare ~compare:(List.compare ~compare:compare_type_value) a b
and compare_type_value { tsrc = _ ; t = ta } { tsrc = _ ; t = tb } =
  (* Note: this comparison ignores the tsrc, the idea is that types
     will often be compared to see if they are the same, regardless of
     where the type comes from .*)
  compare_type_expression_ ta tb

and compare_type_expression_ = function
  | P_forall { binder=a1; constraints=a2; body=a3 } -> (function
      | P_forall { binder=b1; constraints=b2; body=b3 } ->
        compare_type_variable a1 b1 <? fun () ->
          List.compare ~compare:compare_type_constraint a2 b2  <? fun () ->
            compare_type_value a3 b3
      | P_variable _ -> -1
      | P_constant _ -> -1
      | P_row      _ -> -1
      | P_apply _    -> -1)
  | P_variable a -> (function
      | P_forall _ -> 1
      | P_variable b -> compare_type_variable a b
      | P_constant _ -> -1
      | P_row      _ -> -1
      | P_apply _ -> -1)
  | P_constant { p_ctor_tag=a1; p_ctor_args=a2 } -> (function
      | P_forall _ -> 1
      | P_variable _ -> 1
      | P_constant { p_ctor_tag=b1; p_ctor_args=b2 } -> compare_simple_c_constant a1 b1 <? fun () -> List.compare ~compare:compare_type_value a2 b2
      | P_row      _ -> -1
      | P_apply _ -> -1)
  | P_row { p_row_tag=a1; p_row_args=a2 } -> (function
      | P_forall   _ -> 1
      | P_variable _ -> 1
      | P_constant _ -> 1
      | P_row { p_row_tag=b1; p_row_args=b2 } -> compare_simple_c_row a1 b1 <? fun () -> compare_lmap compare_type_value a2 b2
      | P_apply    _ -> -1)
  | P_apply { tf=a1; targ=a2 } -> (function
      | P_forall _ -> 1
      | P_variable _ -> 1
      | P_constant _ -> 1
      | P_row      _ -> 1
      | P_apply { tf=b1; targ=b2 } -> compare_type_value a1 b1 <? fun () -> compare_type_value a2 b2)
and compare_type_constraint = fun { c = ca ; reason = ra } { c = cb ; reason = rb } ->
  let c = compare_type_constraint_ ca cb in
  if c = 0 then String.compare ra rb
  else c

and compare_type_constraint_ = function
  | C_equation { aval=a1; bval=a2 } -> (function
      | C_equation { aval=b1; bval=b2 } -> compare_type_value a1 b1 <? fun () -> compare_type_value a2 b2
      | C_typeclass _ -> -1
      | C_access_label _ -> -1)
  | C_typeclass { tc_args=a1; typeclass=a2 } -> (function
      | C_equation _ -> 1
      | C_typeclass { tc_args=b1; typeclass=b2 } -> List.compare ~compare:compare_type_value a1 b1 <? fun () -> compare_typeclass a2 b2
      | C_access_label _ -> -1)
  | C_access_label { c_access_label_tval=a1; accessor=a2; c_access_label_tvar=a3 } -> (function
      | C_equation _ -> 1
      | C_typeclass _ -> 1
      | C_access_label { c_access_label_tval=b1; accessor=b2; c_access_label_tvar=b3 } -> compare_type_value a1 b1 <? fun () -> compare_label a2 b2  <? fun () -> compare_type_variable a3 b3)
let compare_type_constraint_list = List.compare ~compare:compare_type_constraint
let compare_p_forall
    { binder = a1; constraints = a2; body = a3 }
    { binder = b1; constraints = b2; body = b3 } =
  compare_type_variable a1 b1 <? fun () ->
    compare_type_constraint_list a2 b2 <? fun () ->
      compare_type_value a3 b3
let compare_c_poly_simpl { tv = a1; forall = a2 } { tv = b1; forall = b2 } =
  compare_type_variable a1 b1 <? fun () ->
    compare_p_forall a2 b2

let compare_c_constructor_simpl { reason_constr_simpl = _ ; tv=a1; c_tag=a2; tv_list=a3 } { reason_constr_simpl = _ ; tv=b1; c_tag=b2; tv_list=b3 } =
  (* We do not compare the reasons, as they are only for debugging and
     not part of the type *)
  compare_type_variable a1 b1 <? fun () -> compare_simple_c_constant a2 b2  <? fun () -> List.compare ~compare:compare_type_variable a3 b3

(* TODO: use Ast_typed.Compare_generic.output_specialize1 etc. but don't compare the reasons *)
let compare_output_specialize1 { poly = a1; a_k_var = a2 } { poly = b1; a_k_var = b2 } =
  compare_c_poly_simpl a1 b1 <? fun () ->
    compare_c_constructor_simpl a2 b2

let compare_output_break_ctor { a_k_var=a1; a_k'_var'=a2 } { a_k_var=b1; a_k'_var'=b2 } =
  compare_c_constructor_simpl a1 b1 <? fun () -> compare_c_constructor_simpl a2 b2

let compare_c_typeclass_simpl_args =
  List.compare ~compare:Var.compare

let compare_c_typeclass_simpl
    { reason_typeclass_simpl = _ ; tc = a1 ; args = a2 }
    { reason_typeclass_simpl = _ ; tc = b1 ; args = b2 } =
  compare_typeclass a1 b1 <? fun () -> compare_c_typeclass_simpl_args a2 b2

(* Using a pretty-printer from the PP.ml module creates a dependency
   loop, so the one that we need temporarily for debugging purposes
   has been copied here. *)
let debug_pp_constant : _ -> constant_tag -> unit = fun ppf c_tag ->
    let ct = match c_tag with
      | T.C_arrow     -> "arrow"
      | T.C_option    -> "option"
      | T.C_map       -> "map"
      | T.C_big_map   -> "big_map"
      | T.C_list      -> "list"
      | T.C_set       -> "set"
      | T.C_unit      -> "unit"
      | T.C_string    -> "string"
      | T.C_nat       -> "nat"
      | T.C_mutez     -> "mutez"
      | T.C_timestamp -> "timestamp"
      | T.C_int       -> "int"
      | T.C_address   -> "address"
      | T.C_bytes     -> "bytes"
      | T.C_key_hash  -> "key_hash"
      | T.C_key       -> "key"
      | T.C_signature -> "signature"
      | T.C_operation -> "operation"
      | T.C_contract  -> "contract"
      | T.C_chain_id  -> "chain_id"
    in
    Format.fprintf ppf "%s" ct

let debug_pp_c_constructor_simpl ppf { tv; c_tag; tv_list } =
  Format.fprintf ppf "CTOR %a %a(%a)" Var.pp tv debug_pp_constant c_tag PP_helpers.(list_sep Var.pp (const " , ")) tv_list
