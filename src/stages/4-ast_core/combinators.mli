open Types

val make_t      : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> type_content -> type_expression
val t_bool      : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_string    : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_bytes     : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_int       : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_operation : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_nat       : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_tez       : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_unit      : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_address   : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_key       : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_key_hash  : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_timestamp : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
val t_signature : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> unit -> type_expression
(*
val t_option    : type_expression -> type_expression
*)
val t_list      : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> type_expression -> type_expression
val t_variable  : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> string -> type_expression
(*
val t_record    : te_map -> type_expression
*)
val t_pair   : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> ( row_element * row_element ) -> type_expression
val t_tuple  : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> row_element list -> type_expression

val t_record    : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> row_element Map.String.t -> type_expression
val t_record_ez : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> (string * row_element) list -> type_expression

val t_sum    : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> Types.row_element Map.String.t -> type_expression
val ez_t_sum : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> ( string * Types.row_element ) list -> type_expression

val t_function : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> type_expression -> type_expression -> type_expression

val t_map : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> type_expression -> type_expression -> type_expression
val t_big_map : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> type_expression -> type_expression -> type_expression
val t_contract : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> type_expression -> type_expression
val t_set      : ?loc:Location.t -> ?sugar:Ast_sugar.type_expression -> type_expression -> type_expression

val make_e         : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression_content -> expression
val e_var          : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression
val e_literal      : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> literal -> expression
val e_unit         : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> unit -> expression
val e_int          : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> Z.t -> expression 
val e_nat          : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> Z.t -> expression
val e_timestamp    : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> Z.t -> expression
val e_bool         : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> bool -> expression
val e_string       : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> ligo_string -> expression
val e_address      : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression 
val e_signature    : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression 
val e_key          : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression 
val e_key_hash     : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression 
val e_chain_id     : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression 
val e_mutez        : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> Z.t -> expression
val e'_bytes       : string -> expression_content
val e_bytes_hex    : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression
val e_bytes_raw    : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> bytes -> expression
val e_bytes_string : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression

val e_some            : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> expression
val e_none            : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> unit -> expression
val e_string_cat      : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> expression -> expression
val e_map_add         : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> expression ->  expression -> expression
val e_constructor     : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression -> expression
val e_matching        : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> matching_expr -> expression
val e_record_accessor : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> label -> expression
val e_variable        : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression_variable -> expression
val e_let_in          : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> binder -> bool -> expression -> expression -> expression
val e_raw_code        : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> string -> expression -> expression
val e_annotation      : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> type_expression -> expression
val e_application     : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> expression -> expression
val e_constant        : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> constant' -> expression list -> expression

val make_option_typed : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> type_expression option -> expression

val e_typed_none      : ?loc:Location.t -> type_expression -> expression

val e_lambda        : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> binder -> expression -> expression
val e_recursive     : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression_variable -> type_expression -> lambda -> expression
val e_record        : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression label_map-> expression
val e_record_update : ?loc:Location.t -> ?sugar:Ast_sugar.expression -> expression -> label -> expression -> expression

val assert_e_record_accessor : expression_content -> unit option

val get_e_pair : expression_content -> (expression * expression) option
val get_e_list : expression_content -> expression list option
val get_e_tuple : expression_content -> expression list option
val get_e_ascription : expression_content -> (expression * type_expression) option

val extract_pair : expression -> (expression * expression) option
val extract_record : expression -> (label * expression) list option
val extract_map : expression -> (expression * expression) list option
