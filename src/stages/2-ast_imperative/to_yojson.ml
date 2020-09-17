open Types

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

let label = label_to_yojson
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

let deprecated {name;const} = 
  `Assoc [
    ("name", `String name);
    ("const", constant' const);
  ]

let rich_constant = function
  | Deprecated d -> `List [`String "Deprecatd"; deprecated d ]
  | Const      c -> `List [`String "Const"; constant' c ]


let rec type_expression {type_content=tc;location} =
  `Assoc [
    ("type_content", type_content tc);
    ("location", Location.to_yojson location);
  ]

and type_content = function
  | T_sum      t -> `List [ `String "t_sum"; label_map row_element t]
  | T_record   t -> `List [ `String "t_record"; label_map row_element t]
  | T_tuple    t -> `List [ `String "t_tuple";  list type_expression t]
  | T_arrow    t -> `List [ `String "t_arrow"; arrow t]
  | T_variable t -> `List [ `String "t_variable"; type_variable_to_yojson t]
  | T_constant t -> `List [ `String "t_constant"; type_operator t]
  | T_wildcard   -> `List [ `String "t_wildcard"; `Null]
  | T_annoted  t -> `List [ `String "t_annoted"; `List [type_expression @@ fst t;`String (snd t)]]

and row_element {associated_type; decl_pos} =
  `Assoc [
    ("associated_type", type_expression associated_type);
    ("decl_pos", `Int decl_pos);
  ]

and arrow {type1;type2} =
  `Assoc [
    ("type1", type_expression type1);
    ("type2", type_expression type2);
  ]

and type_operator (tc, arguments) =
  `Assoc [
    ("type_constant", type_constant tc);
    ("arguments", list type_expression arguments)
  ]

let rec expression {expression_content=ec;location} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("location", Location.to_yojson location);
  ]

and expression_content = function 
  (* Base *)
  | E_literal     e -> `List [ `String "E_literal"; literal e ]
  | E_constant    e -> `List [ `String "E_constant"; constant e ]
  | E_variable    e -> `List [ `String "E_variable"; expression_variable_to_yojson e ]
  | E_application e -> `List [ `String "E_application"; application e ]
  | E_lambda      e -> `List [ `String "E_lambda"; lambda e ]
  | E_recursive   e -> `List [ `String "E_recursive"; recursive e ]
  | E_let_in      e -> `List [ `String "E_let_in"; let_in e ]
  | E_raw_code    e -> `List [ `String "E_raw_code"; raw_code e ]
  (* Variant *)
  | E_constructor e -> `List [ `String "E_constructor"; constructor e ]
  | E_matching    e -> `List [ `String "E_matching"; matching e ]
  (* Record *)
  | E_record      e -> `List [ `String "E_record"; record e ]
  | E_accessor    e -> `List [ `String "E_record_accessor"; accessor e ]
  | E_update      e -> `List [ `String "E_record_update"; update e ]
  | E_ascription  e -> `List [ `String "E_ascription"; ascription e ]
  | E_cond        e -> `List [ `String "E_cond"; conditional e ]
  | E_sequence    e -> `List [ `String "E_sequence"; sequence e ]
  | E_skip          -> `List [ `String "E_skip"; `Null ]
  | E_tuple       e -> `List [ `String "E_tuple"; list expression e ]
  (* Data Structures *)
  | E_map         e -> `List [ `String "E_map"; list (fun (k,v) -> `List [ expression k; expression v]) e ] 
  | E_big_map     e -> `List [ `String "E_big_map"; list (fun (k,v) -> `List [ expression k; expression v]) e ]
  | E_list        e -> `List [ `String "E_list"; list expression e]
  | E_set         e -> `List [ `String "E_set"; list expression e]
  | E_assign      e -> `List [ `String "E_assign"; assign e ]
  | E_for         e -> `List [ `String "E_for"; for_ e ]
  | E_for_each    e -> `List [ `String "E_for_each"; for_each e ]
  | E_while       e -> `List [ `String "E_while"; while_loop e ]


and constant {cons_name;arguments} =
  `Assoc [
    ("cons_name", rich_constant cons_name);
    ("arguments", list expression arguments);
  ]

and application {lamb;args} =
  `Assoc [
    ("lamb", expression lamb);
    ("args", expression args);
  ]

and lambda {binder;result} =
  `Assoc [
    ("binder", expression_variable_to_yojson @@ fst binder);
    ("result", expression result);
  ]

and recursive {fun_name;fun_type;lambda=l} =
  `Assoc [
    ("fun_name", expression_variable_to_yojson fun_name);
    ("fun_type", type_expression fun_type);
    ("lambda", lambda l)
  ]

and let_in {let_binder;rhs;let_result;inline} =
  `Assoc [
    ("let_binder", expression_variable_to_yojson @@ fst let_binder);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("inline", `Bool inline);
  ]

and raw_code {language;code} =
  `Assoc [
    ("language", `String language);
    ("code", expression code);
  ]

and constructor {constructor;element} =
  `Assoc [
    ("constructor", label constructor);
    ("element", expression element);
  ]

and matching {matchee; cases} =
  `Assoc [
    ("matchee", expression matchee);
    ("cases", matching_expr cases);
  ]

and record r = label_map expression r

and accessor {record; path} =
  `Assoc [
    ("record", expression record);
    ("path", list access path);
  ]

and update {record; path; update} =
  `Assoc [
    ("record", expression record);
    ("path", list access path);
    ("update", expression update);
  ]
and access = function
  | Access_tuple  a -> `List [ `String "Access_tuple"; z_to_yojson a]
  | Access_record a -> `List [ `String "Access_tuple"; `String a]
  | Access_map    a -> `List [ `String "Access_tuple"; expression a]

and ascription {anno_expr; type_annotation} =
  `Assoc [
    ("anno_expr", expression anno_expr);
    ("type_annotation", type_expression type_annotation);
  ]
and conditional {condition; then_clause; else_clause} =
  `Assoc [
    ("condition", expression condition);
    ("then_clause", expression then_clause);
    ("else_clause", expression else_clause);
  ]
and sequence {expr1;expr2} =
  `Assoc [
    ("expr1", expression expr1);
    ("expr2", expression expr2);
  ]

and assign {variable; access_path; expression=e} =
  `Assoc [
    ("variable", expression_variable_to_yojson variable);
    ("access_path", list access access_path);
    ("expression", expression e);
  ]

and for_ {binder; start; final; increment; body} =
  `Assoc [
    ("binder", expression_variable_to_yojson binder);
    ("start", expression start);
    ("final", expression final);
    ("increment", expression increment);
    ("body", expression body);
  ]

and for_each {binder;collection;collection_type;body} =
  `Assoc [
    ("binder", expression_variable_to_yojson @@ fst binder);
    ("collection", expression collection);
    ("collection_type", collect_type collection_type);
    ("body", expression body);
  ]

and collect_type = function
  | Map  -> `List [ `String "Map"; `Null] 
  | Set  -> `List [ `String "Set"; `Null] 
  | List -> `List [ `String "List"; `Null]  

and while_loop {condition;body} =
  `Assoc [
    ("condition", expression condition);
    ("body", expression body);
  ]
and matching_expr = function
  | Match_list    {match_nil;match_cons} -> `List [ `String "Match_list";    
    `Assoc [
      ("match_nil", expression match_nil);
      ("match_cons", matching_content_cons match_cons);
    ]]
  | Match_option  {match_none;match_some} -> `List [ `String "Match_option";
    `Assoc [
      ("match_none", expression match_none);
      ("match_some", matching_content_some match_some);
    ]]
  | Match_variant m -> `List [ `String "Match_variant"; list matching_content_case m ]
  | Match_tuple   (lst,e) -> `List [ `String "Match_tuple";
    `List [
      list (fun (e,t) -> `List [expression_variable_to_yojson e; type_expression t]) lst;
      expression e;
    ]]
  | Match_record (lst, e) -> `List [`String "Match_record";
    `List [
      list (fun (l,e,t) -> `List [label l; expression_variable_to_yojson e; type_expression t]) lst;
      expression e;
    ]]
  | Match_variable ((ev,t),e) -> `List [`String "Match_varible";
    `List [expression_variable_to_yojson ev; type_expression t; expression e];
    ]

and matching_content_cons (hd, tl, body) =
  `Assoc [
    ("hd", expression_variable_to_yojson hd);
    ("tl", expression_variable_to_yojson tl);
    ("body", expression body);
  ]

and matching_content_some (opt, body ) =
  `Assoc [
    ("opt", expression_variable_to_yojson opt);
    ("body", expression body);
  ]

and matching_content_case ((constructor, pattern), body) =
  `Assoc [
    ("constructor", label constructor);
    ("pattern", expression_variable_to_yojson pattern);
    ("body", expression body);
  ]

let declaration_type (type_binder, type_expr) =
  `Assoc [
    ("type_binder", type_variable_to_yojson type_binder);
    ("type_expr", type_expression type_expr);
  ]

let declaration_constant (binder,ty,inline,expr) =
  `Assoc [
    ("binder",expression_variable_to_yojson binder);
    ("type_expression", type_expression ty);
    ("expr", expression expr);
    ("attribute", `Bool inline);
  ]
let declaration = function 
  | Declaration_type     dt -> `List [ `String "Declaration_type"; declaration_type dt]
  | Declaration_constant dc -> `List [ `String "Declaration_constant"; declaration_constant dc]

let program = list (Location.wrap_to_yojson declaration)
