open Trace
open Types

val make_n_e : name -> annotated_expression -> named_expression
val make_n_t : name -> type_value -> named_type_value
val make_t : type_value' -> S.type_expression option -> type_value
val make_a_e : ?location:Location.t -> expression -> type_value -> full_environment -> annotated_expression

val t_bool : ?s:S.type_expression -> unit -> type_value
val t_string : ?s:S.type_expression -> unit -> type_value
val t_bytes : ?s:S.type_expression -> unit -> type_value
val t_key : ?s:S.type_expression -> unit -> type_value
val t_key_hash : ?s:S.type_expression -> unit -> type_value
val t_operation : ?s:S.type_expression -> unit -> type_value
val t_timestamp : ?s:S.type_expression -> unit -> type_value
val t_set : type_value -> ?s:S.type_expression -> unit -> type_value
val t_contract : type_value -> ?s:S.type_expression -> unit -> type_value
val t_int : ?s:S.type_expression -> unit -> type_value
val t_nat : ?s:S.type_expression -> unit -> type_value
val t_mutez : ?s:S.type_expression -> unit -> type_value
val t_address : ?s:S.type_expression -> unit -> type_value
val t_unit : ?s:S.type_expression -> unit -> type_value
val t_option : type_value -> ?s:S.type_expression -> unit -> type_value
val t_pair : type_value -> type_value -> ?s:S.type_expression -> unit -> type_value
val t_list  : type_value -> ?s:S.type_expression -> unit -> type_value
val t_tuple : type_value list -> ?s:S.type_expression -> unit -> type_value
val t_variable : name -> ?s:S.type_expression -> unit -> type_value
val t_record : tv_map -> ?s:S.type_expression -> unit -> type_value
val make_t_ez_record : (string * type_value) list -> type_value 
(*
val ez_t_record : ( string * type_value ) list -> ?s:S.type_expression -> unit -> type_value 
*)

val t_map : type_value -> type_value -> ?s:S.type_expression -> unit -> type_value
val t_big_map : type_value -> type_value -> ?s:S.type_expression -> unit -> type_value
val t_sum : tv_map -> ?s:S.type_expression -> unit -> type_value
val make_t_ez_sum : ( string * type_value ) list -> type_value
val t_function : type_value -> type_value -> ?s:S.type_expression -> unit -> type_value
val t_shallow_closure : type_value -> type_value -> ?s:S.type_expression -> unit -> type_value
val get_type_annotation : annotated_expression -> type_value
val get_type' : type_value -> type_value'
val get_environment : annotated_expression -> full_environment
val get_expression : annotated_expression -> expression
val get_lambda : expression -> lambda result
val get_lambda_with_type : annotated_expression -> (lambda * ( tv * tv) ) result
val get_t_bool : type_value -> unit result
(*
val get_t_int : type_value -> unit result
val get_t_nat : type_value -> unit result
val get_t_unit : type_value -> unit result
val get_t_mutez : type_value -> unit result
val get_t_bytes : type_value -> unit result 
val get_t_string : type_value -> unit result
*)
val get_t_contract : type_value -> type_value result
val get_t_option : type_value -> type_value result
val get_t_list : type_value -> type_value result 
val get_t_set : type_value -> type_value result
(*
val get_t_key : type_value -> unit result
val get_t_signature : type_value -> unit result
val get_t_key_hash : type_value -> unit result
*)
val get_t_tuple : type_value -> type_value list result
val get_t_pair : type_value -> ( type_value * type_value ) result
val get_t_function : type_value -> ( type_value * type_value ) result
val get_t_sum : type_value -> type_value SMap.t result
val get_t_record : type_value -> type_value SMap.t result
val get_t_map : type_value -> ( type_value * type_value ) result
val get_t_big_map : type_value -> ( type_value * type_value ) result
val get_t_map_key : type_value -> type_value result
val get_t_map_value : type_value -> type_value result
val get_t_big_map_key : type_value -> type_value result
val get_t_big_map_value : type_value -> type_value result

val assert_t_map : type_value -> unit result

val is_t_map : type_value -> bool
val is_t_big_map : type_value -> bool 

val assert_t_mutez : type_value -> unit result
val assert_t_key : type_value -> unit result
val assert_t_signature : type_value -> unit result
val assert_t_key_hash : type_value -> unit result

val assert_t_list : type_value -> unit result

val is_t_list   : type_value -> bool
val is_t_set    : type_value -> bool
val is_t_nat    : type_value -> bool
val is_t_string : type_value -> bool
val is_t_bytes  : type_value -> bool
val is_t_int    : type_value -> bool

val assert_t_bytes : type_value -> unit result
(*
val assert_t_operation : type_value -> unit result
*)
val assert_t_list_operation : type_value -> unit result
val assert_t_int : type_value -> unit result
val assert_t_nat : type_value -> unit result
val assert_t_bool : type_value -> unit result
val assert_t_unit : type_value -> unit result
(*
val e_record : ae_map -> expression
val ez_e_record : ( string * annotated_expression ) list -> expression

*)
val e_some : value -> expression
val e_none : expression
val e_map : ( value * value ) list -> expression
val e_unit : expression
val e_int : int -> expression
val e_nat : int -> expression
val e_mutez : int -> expression
val e_bool : bool -> expression
val e_string : string -> expression
val e_bytes : bytes -> expression
val e_timestamp : int -> expression
val e_address : string -> expression
val e_operation : Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation -> expression
val e_lambda : lambda -> expression
val e_pair : value -> value -> expression
val e_application : value -> value -> expression
val e_variable : name -> expression
val e_list : value list -> expression
val e_let_in : string -> value -> value -> expression
val e_tuple : value list -> expression

val e_a_unit : full_environment -> annotated_expression
val e_a_int : int -> full_environment -> annotated_expression
val e_a_nat : int -> full_environment -> annotated_expression
val e_a_mutez : int -> full_environment -> annotated_expression
val e_a_bool : bool -> full_environment -> annotated_expression
val e_a_string : string -> full_environment -> annotated_expression
val e_a_address : string -> full_environment -> annotated_expression
val e_a_pair : annotated_expression -> annotated_expression -> full_environment -> annotated_expression
val e_a_some : annotated_expression -> full_environment -> annotated_expression
val e_a_lambda : lambda -> tv -> tv -> full_environment -> annotated_expression
val e_a_none : type_value -> full_environment -> annotated_expression
val e_a_tuple : annotated_expression list -> full_environment -> annotated_expression
val e_a_record : ae_map -> full_environment -> annotated_expression
val e_a_application : annotated_expression -> annotated_expression -> full_environment -> annotated_expression
val e_a_variable : name -> type_value -> full_environment -> annotated_expression
val ez_e_a_record : ( name * annotated_expression ) list -> full_environment -> annotated_expression
val e_a_map : ( annotated_expression * annotated_expression ) list -> type_value -> type_value -> full_environment -> annotated_expression
val e_a_list : annotated_expression list -> type_value -> full_environment -> annotated_expression
val e_a_let_in : name -> annotated_expression -> annotated_expression -> full_environment -> annotated_expression

val get_a_int : annotated_expression -> int result
val get_a_unit : annotated_expression -> unit result
val get_a_bool : annotated_expression -> bool result
val get_a_record_accessor : annotated_expression -> (annotated_expression * name) result
val get_declaration_by_name : program -> string -> declaration result
