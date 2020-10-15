open Types
open Yojson_helpers

type json = Yojson.Safe.t

let type_constant = function
  | TC_unit                      -> `List [ `String "TC_unit"; `Null]
  | TC_string                    -> `List [ `String "TC_string"; `Null]
  | TC_bytes                     -> `List [ `String "TC_bytes"; `Null]
  | TC_nat                       -> `List [ `String "TC_nat"; `Null]
  | TC_int                       -> `List [ `String "TC_int"; `Null]
  | TC_mutez                     -> `List [ `String "TC_mutez"; `Null]
  | TC_operation                 -> `List [ `String "TC_operation"; `Null]
  | TC_address                   -> `List [ `String "TC_address"; `Null]
  | TC_key                       -> `List [ `String "TC_key"; `Null]
  | TC_key_hash                  -> `List [ `String "TC_key_hash"; `Null]
  | TC_chain_id                  -> `List [ `String "TC_chain_id"; `Null]
  | TC_signature                 -> `List [ `String "TC_signature"; `Null]
  | TC_timestamp                 -> `List [ `String "TC_timestamp"; `Null]
  | TC_contract                  -> `List [ `String "TC_contract"; `Null]
  | TC_option                    -> `List [ `String "TC_option"; `Null]
  | TC_list                      -> `List [ `String "TC_list"; `Null]
  | TC_set                       -> `List [ `String "TC_set"; `Null]
  | TC_map                       -> `List [ `String "TC_map"; `Null]
  | TC_big_map                   -> `List [ `String "TC_big_map"; `Null]
  | TC_map_or_big_map            -> `List [ `String "TC_map_or_big_map"; `Null]
  | TC_michelson_pair            -> `List [ `String "TC_michelson_pair"; `Null]
  | TC_michelson_or              -> `List [ `String "TC_michelson_or"; `Null]
  | TC_michelson_pair_right_comb -> `List [ `String "TC_michelson_pair_right_comb"; `Null]
  | TC_michelson_pair_left_comb  -> `List [ `String "TC_michelson_pair_left_comb"; `Null]
  | TC_michelson_or_right_comb   -> `List [ `String "TC_michelson_or_right_comb"; `Null]
  | TC_michelson_or_left_comb    -> `List [ `String "TC_michelson_or_left_comb"; `Null]

let constant' = function
  | C_INT                -> `List [`String "C_INT"; `Null ]
  | C_UNIT               -> `List [`String "C_UNIT"; `Null ]
  | C_NIL                -> `List [`String "C_NIL"; `Null ]
  | C_NOW                -> `List [`String "C_NOW"; `Null ]
  | C_IS_NAT             -> `List [`String "C_IS_NAT"; `Null ]
  | C_SOME               -> `List [`String "C_SOME"; `Null ]
  | C_NONE               -> `List [`String "C_NONE"; `Null ]
  | C_ASSERTION          -> `List [`String "C_ASSERTION"; `Null ]
  | C_ASSERT_SOME        -> `List [`String "C_ASSERT_SOME"; `Null ]
  | C_ASSERT_INFERRED    -> `List [`String "C_ASSERT_INFERRED"; `Null ]
  | C_FAILWITH           -> `List [`String "C_FAILWITH"; `Null ]
  | C_UPDATE             -> `List [`String "C_UPDATE"; `Null ]
  (* Loops *)
  | C_ITER               -> `List [`String "C_ITER"; `Null ]
  | C_FOLD_WHILE         -> `List [`String "C_FOLD_WHILE"; `Null ]
  | C_FOLD_CONTINUE      -> `List [`String "C_FOLD_CONTINUE"; `Null ]
  | C_FOLD_STOP          -> `List [`String "C_FOLD_STOP"; `Null ]
  | C_LOOP_LEFT          -> `List [`String "C_LOOP_LEFT"; `Null ]
  | C_LOOP_CONTINUE      -> `List [`String "C_LOOP_CONTINUE"; `Null ]
  | C_LOOP_STOP          -> `List [`String "C_LOOP_STOP"; `Null ]
  | C_FOLD               -> `List [`String "C_FOLD"; `Null ]
  (* MATH *)
  | C_NEG                -> `List [`String "C_NEG"; `Null ]
  | C_ABS                -> `List [`String "C_ABS"; `Null ]
  | C_ADD                -> `List [`String "C_ADD"; `Null ]
  | C_SUB                -> `List [`String "C_SUB"; `Null ]
  | C_MUL                -> `List [`String "C_MUL"; `Null ]
  | C_EDIV               -> `List [`String "C_EDIV"; `Null ]
  | C_DIV                -> `List [`String "C_DIV"; `Null ]
  | C_MOD                -> `List [`String "C_MOD"; `Null ]
  (* LOGIC *)
  | C_NOT                -> `List [`String "C_NOT"; `Null ]
  | C_AND                -> `List [`String "C_AND"; `Null ]
  | C_OR                 -> `List [`String "C_OR"; `Null ]
  | C_XOR                -> `List [`String "C_XOR"; `Null ]
  | C_LSL                -> `List [`String "C_LSL"; `Null ]
  | C_LSR                -> `List [`String "C_LSR"; `Null ]
  (* COMPARATOR *)
  | C_EQ                 -> `List [`String "C_EQ"; `Null ]
  | C_NEQ                -> `List [`String "C_NEQ"; `Null ]
  | C_LT                 -> `List [`String "C_LT"; `Null ]
  | C_GT                 -> `List [`String "C_GT"; `Null ]
  | C_LE                 -> `List [`String "C_LE"; `Null ]
  | C_GE                 -> `List [`String "C_GE"; `Null ]
  (* Bytes/ String *)
  | C_SIZE               -> `List [`String "C_SIZE"; `Null ]
  | C_CONCAT             -> `List [`String "C_CONCAT"; `Null ]
  | C_SLICE              -> `List [`String "C_SLICE"; `Null ]
  | C_BYTES_PACK         -> `List [`String "C_BYTES_PACK"; `Null ]
  | C_BYTES_UNPACK       -> `List [`String "C_BYTES_UNPACK"; `Null ]
  | C_CONS               -> `List [`String "C_CONS"; `Null ]
  (* Pair *)
  | C_PAIR               -> `List [`String "C_PAIR"; `Null ]
  | C_CAR                -> `List [`String "C_CAR"; `Null ]
  | C_CDR                -> `List [`String "C_CDR"; `Null ]
  | C_TRUE               -> `List [`String "C_TRUE"; `Null ]
  | C_FALSE              -> `List [`String "C_FALSE"; `Null ]
  | C_LEFT               -> `List [`String "C_LEFT"; `Null ]
  | C_RIGHT              -> `List [`String "C_RIGHT"; `Null ]
  (* Set *)
  | C_SET_EMPTY          -> `List [`String "C_SET_EMPTY"; `Null ]
  | C_SET_LITERAL        -> `List [`String "C_SET_LITERAL"; `Null ]
  | C_SET_ADD            -> `List [`String "C_SET_ADD"; `Null ]
  | C_SET_REMOVE         -> `List [`String "C_SET_REMOVE"; `Null ]
  | C_SET_ITER           -> `List [`String "C_SET_ITER"; `Null ]
  | C_SET_FOLD           -> `List [`String "C_SET_FOLD"; `Null ]
  | C_SET_MEM            -> `List [`String "C_SET_MEM"; `Null ]
  (* List *)
  | C_LIST_EMPTY         -> `List [`String "C_LIST_EMPTY"; `Null ]
  | C_LIST_LITERAL       -> `List [`String "C_LIST_LITERAL"; `Null ]
  | C_LIST_ITER          -> `List [`String "C_LIST_ITER"; `Null ]
  | C_LIST_MAP           -> `List [`String "C_LIST_MAP"; `Null ]
  | C_LIST_FOLD          -> `List [`String "C_LIST_FOLD"; `Null ]
  (* Maps *)
  | C_MAP                -> `List [`String "C_MAP"; `Null ]
  | C_MAP_EMPTY          -> `List [`String "C_MAP_EMPTY"; `Null ]
  | C_MAP_LITERAL        -> `List [`String "C_MAP_LITERAL"; `Null ]
  | C_MAP_GET            -> `List [`String "C_MAP_GET"; `Null ]
  | C_MAP_GET_FORCE      -> `List [`String "C_MAP_GET_FORCE"; `Null ]
  | C_MAP_ADD            -> `List [`String "C_MAP_ADD"; `Null ]
  | C_MAP_REMOVE         -> `List [`String "C_MAP_REMOVE"; `Null ]
  | C_MAP_UPDATE         -> `List [`String "C_MAP_UPDATE"; `Null ]
  | C_MAP_ITER           -> `List [`String "C_MAP_ITER"; `Null ]
  | C_MAP_MAP            -> `List [`String "C_MAP_MAP"; `Null ]
  | C_MAP_FOLD           -> `List [`String "C_MAP_FOLD"; `Null ]
  | C_MAP_MEM            -> `List [`String "C_MAP_MEM"; `Null ]
  | C_MAP_FIND           -> `List [`String "C_MAP_FIND"; `Null ]
  | C_MAP_FIND_OPT       -> `List [`String "C_MAP_FIND_OPT"; `Null ]
  (* Big Maps *)
  | C_BIG_MAP            -> `List [`String "C_BIG_MAP"; `Null ]
  | C_BIG_MAP_EMPTY      -> `List [`String "C_BIG_MAP_EMPTY"; `Null ]
  | C_BIG_MAP_LITERAL    -> `List [`String "C_BIG_MAP_LITERAL"; `Null ]
  (* Crypto *)
  | C_SHA256             -> `List [`String "C_SHA256"; `Null ]
  | C_SHA512             -> `List [`String "C_SHA512"; `Null ]
  | C_BLAKE2b            -> `List [`String "C_BLAKE2b"; `Null ]
  | C_HASH               -> `List [`String "C_HASH"; `Null ]
  | C_HASH_KEY           -> `List [`String "C_HASH_KEY"; `Null ]
  | C_CHECK_SIGNATURE    -> `List [`String "C_CHECK_SIGNATURE"; `Null ]
  | C_CHAIN_ID           -> `List [`String "C_CHAIN_ID"; `Null ]
  (* Blockchain *)
  | C_CALL                     -> `List [`String "C_CALL"; `Null ]
  | C_CONTRACT                 -> `List [`String "C_CONTRACT"; `Null ]
  | C_CONTRACT_OPT             -> `List [`String "C_CONTRACT_OPT"; `Null ]
  | C_CONTRACT_ENTRYPOINT      -> `List [`String "C_CONTRACT_ENTRYPOINT"; `Null ]
  | C_CONTRACT_ENTRYPOINT_OPT  -> `List [`String "C_CONTRACT_ENTRYPOINT_OPT"; `Null ]
  | C_AMOUNT                   -> `List [`String "C_AMOUNT"; `Null ]
  | C_BALANCE                  -> `List [`String "C_BALANCE"; `Null ]
  | C_SOURCE                   -> `List [`String "C_SOURCE"; `Null ]
  | C_SENDER                   -> `List [`String "C_SENDER"; `Null ]
  | C_ADDRESS                  -> `List [`String "C_ADDRESS"; `Null ]
  | C_SELF                     -> `List [`String "C_SELF"; `Null ]
  | C_SELF_ADDRESS             -> `List [`String "C_SELF_ADDRESS"; `Null ]
  | C_IMPLICIT_ACCOUNT         -> `List [`String "C_IMPLICIT_ACCOUNT"; `Null ]
  | C_SET_DELEGATE             -> `List [`String "C_SET_DELEGATE"; `Null ]
  | C_CREATE_CONTRACT          -> `List [`String "C_CREATE_CONTRACT"; `Null ]
  | C_CONVERT_TO_LEFT_COMB     -> `List [`String "C_CONVERT_TO_LEFT_COMB"; `Null ]
  | C_CONVERT_TO_RIGHT_COMB    -> `List [`String "C_CONVERT_TO_RIGHT_COMB"; `Null ]
  | C_CONVERT_FROM_LEFT_COMB   -> `List [`String "C_CONVERT_FROM_LEFT_COMB"; `Null ]
  | C_CONVERT_FROM_RIGHT_COMB  -> `List [`String "C_CONVERT_FROM_RIGHT_COMB"; `Null ]

let literal = function
  | Literal_unit        -> `List [`String "Literal_unit"; `Null ]
  | Literal_int       l -> `List [`String "Literal_unit"; z_to_yojson l ]
  | Literal_nat       l -> `List [`String "Literal_unit"; z_to_yojson l ]
  | Literal_timestamp l -> `List [`String "Literal_unit"; z_to_yojson l ]
  | Literal_mutez     l -> `List [`String "Literal_unit"; z_to_yojson l ]
  | Literal_string    l -> `List [`String "Literal_unit"; Ligo_string.to_yojson l ]
  | Literal_bytes     l -> `List [`String "Literal_unit"; bytes_to_yojson l ]
  | Literal_address   l -> `List [`String "Literal_unit"; `String l ]
  | Literal_signature l -> `List [`String "Literal_unit"; `String l ]
  | Literal_key       l -> `List [`String "Literal_unit"; `String l ]
  | Literal_key_hash  l -> `List [`String "Literal_unit"; `String l ]
  | Literal_chain_id  l -> `List [`String "Literal_unit"; `String l ]
  | Literal_operation l -> `List [`String "Literal_unit"; bytes_to_yojson l ]

let label (Label l) = `List [`String "Label"; `String l]
let option f o =
    match o with
    | None   -> `List [ `String "None" ; `Null ]
    | Some v -> `List [ `String "Some" ; f v ]

let list f lst = `List (List.map f lst)

let label_map f lmap =
  let lst = List.sort (fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
  let lst' = List.fold_left
    (fun acc (Label k, v) -> (k , f v)::acc)
    [] lst
  in
  `Assoc lst'

let attributes attr =
  let list = List.map (fun string -> `String string) attr
  in `Assoc [("attributes", `List list)]

let binder type_expression {var;ascr} =
  `Assoc [
    ("var", expression_variable_to_yojson var);
    ("ty", yojson_opt type_expression ascr);
  ]

let row_element g {associated_type; michelson_annotation; decl_pos} =
  `Assoc [
    ("associated_type", g associated_type);
    ("michelson_annotation", option (fun s -> `String s) michelson_annotation);
    ("decl_pos", `Int decl_pos);
  ]

let arrow type_expression {type1;type2} =
  `Assoc [
    ("type1", type_expression type1);
    ("type2", type_expression type2);
  ]

let type_operator type_expression {type_constant=tc; arguments} =
  `Assoc [
    ("type_constant", type_constant tc);
    ("arguments", list type_expression arguments)
  ]


let constant expression {cons_name;arguments} =
  `Assoc [
    ("cons_name", constant' cons_name);
    ("arguments", list expression arguments);
  ]

let application expression {lamb;args} =
  `Assoc [
    ("lamb", expression lamb);
    ("args", expression args);
  ]

let lambda expression type_expression {binder=b;output_type;result} : json =
  `Assoc [
    ("binder", binder type_expression b);
    ("output_type", yojson_opt type_expression output_type);
    ("result", expression result);
  ]

let recursive expression type_expression {fun_name;fun_type;lambda=l} =
  `Assoc [
    ("fun_name", expression_variable_to_yojson fun_name);
    ("fun_type", type_expression fun_type);
    ("lambda", lambda expression type_expression l)
  ]

let let_in expression type_expression {let_binder;rhs;let_result;attributes=attr} =
  `Assoc [
    ("let_binder", binder type_expression let_binder);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("attributes", attributes attr);
  ]

let raw_code expression {language;code} =
  `Assoc [
    ("language", `String language);
    ("code", expression code);
  ]

let constructor expression {constructor;element} =
  `Assoc [
    ("constructor", label constructor);
    ("element", expression element);
  ]

let record expression r = label_map expression r

let access expression = function
  | Access_tuple  a -> `List [ `String "Access_tuple"; z_to_yojson a]
  | Access_record a -> `List [ `String "Access_tuple"; `String a]
  | Access_map    a -> `List [ `String "Access_tuple"; expression a]

let accessor expression ({record; path}: 'exp accessor) =
  `Assoc [
    ("record", expression record);
    ("path", list (access expression) path);
  ]

let record_accessor expression ({record; path}: 'exp record_accessor) =
  `Assoc [
    ("record", expression record);
    ("path", label path);
  ]

let update expression ({record; path; update}: 'exp update) =
  `Assoc [
    ("record", expression record);
    ("path", list (access expression) path);
    ("update", expression update);
  ]

let record_update expression {record; path; update} =
  `Assoc [
    ("record", expression record);
    ("path", label path);
    ("update", expression update);
  ]

let ascription expression type_expression {anno_expr; type_annotation} =
  `Assoc [
    ("anno_expr", expression anno_expr);
    ("type_annotation", type_expression type_annotation);
  ]
let conditional expression {condition; then_clause; else_clause} =
  `Assoc [
    ("condition", expression condition);
    ("then_clause", expression then_clause);
    ("else_clause", expression else_clause);
  ]
let sequence expression {expr1;expr2} =
  `Assoc [
    ("expr1", expression expr1);
    ("expr2", expression expr2);
  ]

let assign expression {variable; access_path; expression=e} =
  `Assoc [
    ("variable", expression_variable_to_yojson variable);
    ("access_path", list (access expression) access_path);
    ("expression", expression e);
  ]

let for_ expression {binder; start; final; incr; f_body} =
  `Assoc [
    ("binder", expression_variable_to_yojson binder);
    ("start" , expression start);
    ("final" , expression final);
    ("incr"  , expression incr);
    ("f_body", expression f_body);
  ]

let collect_type = function
  | Map  -> `List [ `String "Map"; `Null]
  | Set  -> `List [ `String "Set"; `Null]
  | List -> `List [ `String "List"; `Null]

let for_each expression {fe_binder;collection;collection_type;fe_body} =
  `Assoc [
    ("binder",  `List [ expression_variable_to_yojson @@ fst fe_binder; option expression_variable_to_yojson @@ snd fe_binder]);
    ("collection", expression collection);
    ("collection_type", collect_type collection_type);
    ("body", expression fe_body);
  ]

let while_loop expression {cond;body} =
  `Assoc [
    ("cond", expression cond);
    ("body", expression body);
  ]

let declaration_type type_expression {type_binder; type_expr} =
  `Assoc [
    ("type_binder", type_variable_to_yojson type_binder);
    ("type_expr", type_expression type_expr);
  ]

let declaration_constant expression type_expression {binder=b;attr;expr} =
  `Assoc [
    ("binder", binder type_expression b);
    ("expr", expression expr);
    ("attribute", attributes attr);
  ]

let program declaration = list (Location.wrap_to_yojson declaration)
