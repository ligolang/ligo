open Types
open Ligo_prim

val assert_no_type_vars : type_expression -> unit option
val assert_type_expression_eq : type_expression * type_expression -> unit option
val get_entry : module_ -> Value_var.t -> expression option
val get_type_of_contract : type_expression -> (type_expression * type_expression) option
val get_type_of_entrypoint : type_expression -> (type_expression * type_expression) option
val build_entry_type : type_expression -> type_expression -> type_expression

val parameter_from_entrypoints
  :  (Value_var.t * type_expression) Simple_utils.List.Ne.t
  -> ( type_expression * type_expression
     , [> `Not_entry_point_form of Types.expression_variable * Types.type_expression
       | `Storage_does_not_match of
         Value_var.t * Types.type_expression * Value_var.t * Types.type_expression
       ] )
     result

val should_uncurry_entry
  :  Types.type_expression
  -> [> `Bad | `Yes of Types.type_expression * Types.type_expression ]

val fetch_views_in_module
  :  storage_ty:Types.type_expression
  -> Types.module_
  -> Types.module_ * (Types.type_expression * Types.type_expression Binder.t) list

val uncurry_wrap
  :  loc:Location.t
  -> type_:Types.type_expression
  -> Types.expression_variable
  -> Types.expression option

val should_uncurry_view
  :  storage_ty:Types.type_expression
  -> Types.type_expression
  -> [> `Bad
     | `Bad_not_function
     | `Yes of Types.type_expression * Types.type_expression * Types.type_expression
     ]

val to_signature : module_ -> signature
val to_extended_signature : program -> signature
val to_sig_items : module_ -> sig_item list
val get_entrypoint_storage_type : program -> Module_var.t list -> type_expression option

val get_entrypoint_parameter_type
  :  Label.t option
  -> type_expression
  -> type_expression option

val get_contract_signature
  :  signature
  -> Module_var.t list
  -> (signature * contract_sig) option

val get_sig_value
  :  Module_var.t list
  -> Value_var.t
  -> signature
  -> (ty_expr * sig_item_attribute) option

val get_path_signature : signature -> Module_var.t list -> signature option
