open Types

val make_a_e_empty : expression_content -> type_expression -> expression

val e_a_empty_unit : expression
val e_a_empty_int : int -> expression
val e_a_empty_nat : int -> expression
val e_a_empty_mutez : int -> expression
val e_a_empty_bool : bool -> expression
val e_a_empty_string : string -> expression
val e_a_empty_address : string -> expression
val e_a_empty_pair : expression -> expression -> expression
val e_a_empty_some : expression -> expression
val e_a_empty_none : type_expression -> expression
val e_a_empty_record : expression label_map -> expression
val e_a_empty_map : (expression * expression ) list -> type_expression -> type_expression -> expression
val e_a_empty_list : expression list -> type_expression -> expression
val ez_e_a_empty_record : ( label * expression ) list -> expression
val e_a_empty_lambda : lambda -> type_expression -> type_expression -> expression

val env_sum_type : ?env:full_environment -> ?type_name:type_variable -> (constructor' * type_expression) list ->  full_environment
