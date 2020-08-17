open Types

val make_n_t : type_variable -> type_expression -> named_type_content
val make_t : ?loc:Location.t -> type_content -> S.type_expression option -> type_expression
val make_e : ?location:Location.t -> expression_content -> type_expression -> expression

val t_bool      : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_string    : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_bytes     : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_key       : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_key_hash  : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_operation : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_timestamp : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_set       : ?loc:Location.t -> ?core:S.type_expression -> type_expression -> type_expression
val t_contract  : ?loc:Location.t -> ?core:S.type_expression -> type_expression -> type_expression
val t_int       : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_nat       : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_mutez     : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_address   : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_chain_id  : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_signature : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_unit      : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_option    : ?loc:Location.t -> ?core:S.type_expression -> type_expression -> type_expression
val t_pair      : ?loc:Location.t -> ?core:S.type_expression -> type_expression -> type_expression -> type_expression
val t_list      : ?loc:Location.t -> ?core:S.type_expression -> type_expression -> type_expression
val t_variable  : ?loc:Location.t -> ?core:S.type_expression -> type_variable   -> type_expression
val t_wildcard  : ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val t_record    : te_lmap -> ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val make_t_ez_record : ?loc:Location.t -> (string* type_expression) list -> type_expression 
val ez_t_record : ( label * row_element ) list -> ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression 

val t_map            : ?loc:Location.t -> ?core:S.type_expression -> type_expression -> type_expression -> type_expression
val t_big_map        : ?loc:Location.t -> ?core:S.type_expression -> type_expression -> type_expression -> type_expression
val t_map_or_big_map : ?loc:Location.t -> ?core:S.type_expression -> type_expression -> type_expression -> type_expression
val t_sum : Types.te_lmap -> ?loc:Location.t -> ?core:S.type_expression -> unit -> type_expression
val make_t_ez_sum : ?loc:Location.t -> ?core:S.type_expression -> (label * row_element ) list -> type_expression
val t_function : type_expression -> type_expression -> ?loc:Location.t -> ?s:S.type_expression -> unit -> type_expression
val t_shallow_closure : type_expression -> type_expression -> ?loc:Location.t -> ?s:S.type_expression -> unit -> type_expression
val get_type_expression : expression -> type_expression
val get_type' : type_expression -> type_content
val get_expression : expression -> expression_content
val get_lambda : expression -> lambda option
val get_lambda_with_type : expression -> (lambda * ( type_expression * type_expression)) option 
val get_t_bool : type_expression -> unit option
val get_t_contract : type_expression -> type_expression option
val get_t_option : type_expression -> type_expression option
val get_t_list : type_expression -> type_expression option 
val get_t_set : type_expression -> type_expression option
val get_t_tuple : type_expression -> type_expression list option
val get_t_pair : type_expression -> (type_expression * type_expression) option
val get_t_function : type_expression -> (type_expression * type_expression) option
val get_t_function_exn : type_expression -> (type_expression * type_expression)
val get_t_sum     : type_expression -> row_element label_map option
val get_t_sum_exn : type_expression -> row_element label_map
val get_t_record  : type_expression -> row_element label_map option
val get_t_map : type_expression -> (type_expression * type_expression) option
val get_t_big_map : type_expression -> (type_expression * type_expression) option
val get_t_map_key : type_expression -> type_expression option
val get_t_map_value : type_expression -> type_expression option
val get_t_big_map_key : type_expression -> type_expression option
val get_t_big_map_value : type_expression -> type_expression option


val is_t_map : type_expression -> bool
val is_t_big_map : type_expression -> bool 

val assert_t_mutez : type_expression -> unit option
val assert_t_key : type_expression -> unit option
val assert_t_signature : type_expression -> unit option
val assert_t_key_hash : type_expression -> unit option

val is_t_list   : type_expression -> bool
val is_t_set    : type_expression -> bool
val is_t_nat    : type_expression -> bool
val is_t_string : type_expression -> bool
val is_t_bytes  : type_expression -> bool
val is_t_int    : type_expression -> bool

val assert_t_list_operation : type_expression -> unit option
val assert_t_int : type_expression -> unit option
val assert_t_nat : type_expression -> unit option
val assert_t_bool : type_expression -> unit option
val assert_t_unit : type_expression -> unit option
val assert_t_contract : type_expression -> unit option
val assert_t_bytes : type_expression -> unit option
val assert_t_string : type_expression -> unit option
(*
val e_record : ae_map -> expression
val ez_e_record : ( string * expression ) list -> expression

*)
val e_some : expression -> expression_content
val e_none : unit -> expression_content
val e_unit : unit -> expression_content
val e_int : Z.t -> expression_content
val e_nat : Z.t -> expression_content
val e_mutez : Z.t -> expression_content
val e_bool : bool -> expression_content
val e_string : ligo_string -> expression_content
val e_bytes : bytes -> expression_content
val e_timestamp : Z.t -> expression_content
val e_address : string -> expression_content
val e_signature : string -> expression_content
val e_key : string -> expression_content
val e_key_hash : string -> expression_content
val e_chain_id : string -> expression_content
val e_operation : Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation -> expression_content
val e_lambda : lambda -> expression_content
val e_pair : expression -> expression -> expression_content
val e_application : expression -> expression -> expression_content
val e_variable : expression_variable -> expression_content
val e_let_in : expression_variable -> inline -> expression -> expression -> expression_content

val e_a_unit : expression
val e_a_int : Z.t -> expression
val e_a_nat : Z.t -> expression
val e_a_mutez : Z.t -> expression
val e_a_bool : bool -> expression
val e_a_string : ligo_string -> expression
val e_a_address : string -> expression
val e_a_pair : expression -> expression -> expression
val e_a_some : expression -> expression
val e_a_lambda : lambda -> type_expression -> type_expression -> expression
val e_a_none : type_expression -> expression
val e_a_record : expression label_map -> expression
val e_a_application : expression -> expression -> expression
val e_a_variable : expression_variable -> type_expression -> expression
val ez_e_a_record : ( label * expression ) list -> expression
val e_a_let_in : expression_variable -> bool -> expression -> expression -> expression

val get_a_int : expression -> Z.t option
val get_a_string : expression -> string option
val get_a_verbatim : expression -> string option
val get_a_unit : expression -> unit option
val get_a_bool : expression -> bool option
val get_a_record_accessor : expression -> (expression * label) option
val get_declaration_by_name : program -> string -> declaration option
