open Trace
open Types

val make_n_t : type_variable -> type_expression -> named_type_content
val make_t : type_content -> S.type_expression option -> type_expression
val make_a_e : ?location:Location.t -> expression_content -> type_expression -> full_environment -> expression

val t_bool : ?s:S.type_expression -> unit -> type_expression
val t_string : ?s:S.type_expression -> unit -> type_expression
val t_bytes : ?s:S.type_expression -> unit -> type_expression
val t_key : ?s:S.type_expression -> unit -> type_expression
val t_key_hash : ?s:S.type_expression -> unit -> type_expression
val t_operation : ?s:S.type_expression -> unit -> type_expression
val t_timestamp : ?s:S.type_expression -> unit -> type_expression
val t_set : type_expression -> ?s:S.type_expression -> unit -> type_expression
val t_contract : type_expression -> ?s:S.type_expression -> unit -> type_expression
val t_int : ?s:S.type_expression -> unit -> type_expression
val t_nat : ?s:S.type_expression -> unit -> type_expression
val t_mutez : ?s:S.type_expression -> unit -> type_expression
val t_address : ?s:S.type_expression -> unit -> type_expression
val t_chain_id : ?s:S.type_expression -> unit -> type_expression
val t_signature : ?s:S.type_expression -> unit -> type_expression
val t_unit : ?s:S.type_expression -> unit -> type_expression
val t_option : type_expression -> ?s:S.type_expression -> unit -> type_expression
val t_pair : type_expression -> type_expression -> ?s:S.type_expression -> unit -> type_expression
val t_list  : type_expression -> ?s:S.type_expression -> unit -> type_expression
val t_variable : type_variable -> ?s:S.type_expression -> unit -> type_expression
val t_record : type_expression label_map -> ?s:S.type_expression -> unit -> type_expression
val make_t_ez_record : (string* type_expression) list -> type_expression 
val ez_t_record : ( label * type_expression ) list -> ?s:S.type_expression -> unit -> type_expression 

val t_map : type_expression -> type_expression -> ?s:S.type_expression -> unit -> type_expression
val t_big_map : type_expression -> type_expression -> ?s:S.type_expression -> unit -> type_expression
val t_sum : type_expression constructor_map -> ?s:S.type_expression -> unit -> type_expression
val make_t_ez_sum : ( constructor' * type_expression ) list -> type_expression
val t_function : type_expression -> type_expression -> ?s:S.type_expression -> unit -> type_expression
val t_shallow_closure : type_expression -> type_expression -> ?s:S.type_expression -> unit -> type_expression
val get_type_expression : expression -> type_expression
val get_type' : type_expression -> type_content
val get_environment : expression -> full_environment
val get_expression : expression -> expression_content
val get_lambda : expression -> lambda result
val get_lambda_with_type : expression -> (lambda * ( type_expression * type_expression) ) result
val get_t_bool : type_expression -> unit result
(*
val get_t_int : type_expression -> unit result
val get_t_nat : type_expression -> unit result
val get_t_unit : type_expression -> unit result
val get_t_mutez : type_expression -> unit result
val get_t_bytes : type_expression -> unit result 
val get_t_string : type_expression -> unit result
*)
val get_t_contract : type_expression -> type_expression result
val get_t_option : type_expression -> type_expression result
val get_t_list : type_expression -> type_expression result 
val get_t_set : type_expression -> type_expression result
(*
val get_t_key : type_expression -> unit result
val get_t_signature : type_expression -> unit result
val get_t_key_hash : type_expression -> unit result
*)
val get_t_tuple : type_expression -> type_expression list result
val get_t_pair : type_expression -> ( type_expression * type_expression ) result
val get_t_function : type_expression -> ( type_expression * type_expression ) result
val get_t_sum : type_expression -> type_expression constructor_map result
val get_t_record : type_expression -> type_expression label_map result
val get_t_map : type_expression -> ( type_expression * type_expression ) result
val get_t_big_map : type_expression -> ( type_expression * type_expression ) result
val get_t_map_key : type_expression -> type_expression result
val get_t_map_value : type_expression -> type_expression result
val get_t_big_map_key : type_expression -> type_expression result
val get_t_big_map_value : type_expression -> type_expression result

val assert_t_map : type_expression -> unit result

val is_t_map : type_expression -> bool
val is_t_big_map : type_expression -> bool 

val assert_t_mutez : type_expression -> unit result
val assert_t_key : type_expression -> unit result
val assert_t_signature : type_expression -> unit result
val assert_t_key_hash : type_expression -> unit result

val assert_t_list : type_expression -> unit result

val is_t_list   : type_expression -> bool
val is_t_set    : type_expression -> bool
val is_t_nat    : type_expression -> bool
val is_t_string : type_expression -> bool
val is_t_bytes  : type_expression -> bool
val is_t_int    : type_expression -> bool

val assert_t_bytes : type_expression -> unit result
val assert_t_string : type_expression -> unit result
(*
val assert_t_operation : type_expression -> unit result
*)
val assert_t_list_operation : type_expression -> unit result
val assert_t_int : type_expression -> unit result
val assert_t_nat : type_expression -> unit result
val assert_t_bool : type_expression -> unit result
val assert_t_unit : type_expression -> unit result
val assert_t_contract : type_expression -> unit result
(*
val e_record : ae_map -> expression
val ez_e_record : ( string * expression ) list -> expression

*)
val e_some : expression -> expression_content
val e_none : unit -> expression_content
val e_map : ( expression * expression ) list -> expression_content
val e_unit : unit -> expression_content
val e_int : int -> expression_content
val e_nat : int -> expression_content
val e_mutez : int -> expression_content
val e_bool : bool -> expression_content
val e_string : string -> expression_content
val e_bytes : bytes -> expression_content
val e_timestamp : int -> expression_content
val e_address : string -> expression_content
val e_signature : string -> expression_content
val e_key : string -> expression_content
val e_key_hash : string -> expression_content
val e_chain_id : string -> expression_content
val e_operation : Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation -> expression_content
val e_lambda : lambda -> expression_content
val e_pair : expression -> expression -> expression_content
val e_application : expression -> expr -> expression_content
val e_variable : expression_variable -> expression_content
val e_list : expression list -> expression_content
val e_let_in : expression_variable -> inline -> expression -> expression -> expression_content

val e_a_unit : full_environment -> expression
val e_a_int : int -> full_environment -> expression
val e_a_nat : int -> full_environment -> expression
val e_a_mutez : int -> full_environment -> expression
val e_a_bool : bool -> full_environment -> expression
val e_a_string : string -> full_environment -> expression
val e_a_address : string -> full_environment -> expression
val e_a_pair : expression -> expression -> full_environment -> expression
val e_a_some : expression -> full_environment -> expression
val e_a_lambda : lambda -> type_expression -> type_expression -> full_environment -> expression
val e_a_none : type_expression -> full_environment -> expression
val e_a_record : expression label_map -> full_environment -> expression
val e_a_application : expression -> expression -> full_environment -> expression
val e_a_variable : expression_variable -> type_expression -> full_environment -> expression
val ez_e_a_record : ( label * expression ) list -> full_environment -> expression
val e_a_map : ( expression * expression ) list -> type_expression -> type_expression -> full_environment -> expression
val e_a_list : expression list -> type_expression -> full_environment -> expression
val e_a_let_in : expression_variable -> bool -> expression -> expression -> full_environment -> expression

val get_a_int : expression -> int result
val get_a_unit : expression -> unit result
val get_a_bool : expression -> bool result
val get_a_record_accessor : expression -> (expression * label) result
val get_declaration_by_name : program -> string -> declaration result
