open Types
open Format
open PP_helpers

let name ppf (n:expression_variable) : unit = 
  fprintf ppf "%a" Var.pp n

let type_variable ppf (t:type_variable) : unit =
  fprintf ppf "%a" Var.pp t

let constructor ppf (c:constructor) : unit =
  let Constructor c = c in fprintf ppf "%s" c

let label ppf (l:label) : unit =
  let Label l = l in fprintf ppf "%s" l

let constant ppf : constant -> unit = function
  | C_INT                   -> fprintf ppf "INT"
  | C_UNIT                  -> fprintf ppf "UNIT"
  | C_NIL                   -> fprintf ppf "NIL"
  | C_NOW                   -> fprintf ppf "NOW"
  | C_IS_NAT                -> fprintf ppf "IS_NAT"
  | C_SOME                  -> fprintf ppf "SOME"
  | C_NONE                  -> fprintf ppf "NONE"
  | C_ASSERTION             -> fprintf ppf "ASSERTION"
  | C_ASSERT_INFERRED       -> fprintf ppf "ASSERT_INFERRED"
  | C_FAILWITH              -> fprintf ppf "FAILWITH"
  | C_UPDATE                -> fprintf ppf "UPDATE"
  (* Loops *)
  | C_ITER                  -> fprintf ppf "ITER"
  | C_FOLD                  -> fprintf ppf "FOLD"
  | C_FOLD_WHILE            -> fprintf ppf "FOLD_WHILE"
  | C_CONTINUE              -> fprintf ppf "CONTINUE"
  | C_STOP                  -> fprintf ppf "STOP"
  (* MATH *)
  | C_NEG                   -> fprintf ppf "NEG"
  | C_ABS                   -> fprintf ppf "ABS"
  | C_ADD                   -> fprintf ppf "ADD"
  | C_SUB                   -> fprintf ppf "SUB"
  | C_MUL                   -> fprintf ppf "MUL"
  | C_DIV                   -> fprintf ppf "DIV"
  | C_MOD                   -> fprintf ppf "MOD"
  (* LOGIC *)
  | C_NOT                   -> fprintf ppf "NOT"
  | C_AND                   -> fprintf ppf "AND"
  | C_OR                    -> fprintf ppf "OR"
  | C_XOR                   -> fprintf ppf "XOR"
  (* COMPARATOR *)
  | C_EQ                    -> fprintf ppf "EQ"
  | C_NEQ                   -> fprintf ppf "NEQ"
  | C_LT                    -> fprintf ppf "LT"
  | C_GT                    -> fprintf ppf "GT"
  | C_LE                    -> fprintf ppf "LE"
  | C_GE                    -> fprintf ppf "GE"
  (* Bytes/ String *)
  | C_SIZE                  -> fprintf ppf "SIZE"
  | C_CONCAT                -> fprintf ppf "CONCAT"
  | C_SLICE                 -> fprintf ppf "SLICE"
  | C_BYTES_PACK            -> fprintf ppf "BYTES_PACK"
  | C_BYTES_UNPACK          -> fprintf ppf "BYTES_UNPACK"
  | C_CONS                  -> fprintf ppf "CONS"
  (* Pair *)
  | C_PAIR                  -> fprintf ppf "PAIR"
  | C_CAR                   -> fprintf ppf "CAR"
  | C_CDR                   -> fprintf ppf "CDR"
  | C_LEFT                  -> fprintf ppf "LEFT"
  | C_RIGHT                 -> fprintf ppf "RIGHT"
  (* Set *)
  | C_SET_EMPTY             -> fprintf ppf "SET_EMPTY"
  | C_SET_LITERAL           -> fprintf ppf "SET_LITERAL"
  | C_SET_ADD               -> fprintf ppf "SET_ADD"
  | C_SET_REMOVE            -> fprintf ppf "SET_REMOVE"
  | C_SET_ITER              -> fprintf ppf "SET_ITER"
  | C_SET_FOLD              -> fprintf ppf "SET_FOLD"
  | C_SET_MEM               -> fprintf ppf "SET_MEM"
  (* List *)
  | C_LIST_ITER             -> fprintf ppf "LIST_ITER"
  | C_LIST_MAP              -> fprintf ppf "LIST_MAP"
  | C_LIST_FOLD             -> fprintf ppf "LIST_FOLD"
  | C_LIST_CONS             -> fprintf ppf "LIST_CONS"
  (* Maps *)
  | C_MAP                   -> fprintf ppf "MAP"
  | C_MAP_EMPTY             -> fprintf ppf "MAP_EMPTY"
  | C_MAP_LITERAL           -> fprintf ppf "MAP_LITERAL"
  | C_MAP_GET               -> fprintf ppf "MAP_GET"
  | C_MAP_GET_FORCE         -> fprintf ppf "MAP_GET_FORCE"
  | C_MAP_ADD               -> fprintf ppf "MAP_ADD"
  | C_MAP_REMOVE            -> fprintf ppf "MAP_REMOVE"
  | C_MAP_UPDATE            -> fprintf ppf "MAP_UPDATE"
  | C_MAP_ITER              -> fprintf ppf "MAP_ITER"
  | C_MAP_MAP               -> fprintf ppf "MAP_MAP"
  | C_MAP_FOLD              -> fprintf ppf "MAP_FOLD"
  | C_MAP_MEM               -> fprintf ppf "MAP_MEM"
  | C_MAP_FIND              -> fprintf ppf "MAP_FIND"
  | C_MAP_FIND_OPT          -> fprintf ppf "MAP_FIND_OP"
  (* Big Maps *)
  | C_BIG_MAP               -> fprintf ppf "BIG_MAP"
  | C_BIG_MAP_EMPTY         -> fprintf ppf "BIG_MAP_EMPTY"
  | C_BIG_MAP_LITERAL       -> fprintf ppf "BIG_MAP_LITERAL"
  (* Crypto *)
  | C_SHA256                -> fprintf ppf "SHA256"
  | C_SHA512                -> fprintf ppf "SHA512"
  | C_BLAKE2b               -> fprintf ppf "BLAKE2b"
  | C_HASH                  -> fprintf ppf "HASH"
  | C_HASH_KEY              -> fprintf ppf "HASH_KEY"
  | C_CHECK_SIGNATURE       -> fprintf ppf "CHECK_SIGNATURE"
  | C_CHAIN_ID              -> fprintf ppf "CHAIN_ID"
  (* Blockchain *)
  | C_CALL                  -> fprintf ppf "CALL"
  | C_CONTRACT              -> fprintf ppf "CONTRACT"
  | C_CONTRACT_ENTRYPOINT   -> fprintf ppf "CONTRACT_ENTRYPOINT"
  | C_AMOUNT                -> fprintf ppf "AMOUNT"
  | C_BALANCE               -> fprintf ppf "BALANCE"
  | C_SOURCE                -> fprintf ppf "SOURCE"
  | C_SENDER                -> fprintf ppf "SENDER"
  | C_ADDRESS               -> fprintf ppf "ADDRESS"
  | C_SELF_ADDRESS          -> fprintf ppf "SELF_ADDRESS"
  | C_IMPLICIT_ACCOUNT      -> fprintf ppf "IMPLICIT_ACCOUNT"
  | C_SET_DELEGATE          -> fprintf ppf "SET_DELEGATE"
  | C_STEPS_TO_QUOTA        -> fprintf ppf "STEPS_TO_QUOTA"

let cmap_sep value sep ppf m =
  let lst = Types.CMap.to_kv_list m in
  let new_pp ppf (k, v) = fprintf ppf "%a -> %a" constructor k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let lmap_sep value sep ppf m =
  let lst = Types.LMap.to_kv_list m in
  let new_pp ppf (k, v) = fprintf ppf "%a -> %a" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let lrecord_sep value sep ppf m =
  let lst = Types.LMap.to_kv_list m in
  let new_pp ppf (k, v) = fprintf ppf "%a = %a" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let list_sep_d x = list_sep x (const " , ")
let cmap_sep_d x = cmap_sep x (const " , ")
let lmap_sep_d x = lmap_sep x (const " , ")

let rec type_expression' : type a . (formatter -> a -> unit) -> formatter -> a type_expression' -> unit =
  fun f ppf te ->
  match te with
    | T_tuple lst -> fprintf ppf "tuple[%a]" (list_sep_d f) lst
    | T_sum m -> fprintf ppf "sum[%a]" (cmap_sep_d f) m
    | T_record m -> fprintf ppf "record[%a]" (lmap_sep_d f ) m
    | T_arrow (a, b) -> fprintf ppf "%a -> %a" f a f b
    | T_variable tv -> type_variable ppf tv
    | T_constant tc -> type_constant ppf tc
    | T_operator to_ -> type_operator f ppf to_

and type_constant ppf (tc:type_constant) : unit =
  let s = match tc with
    | TC_unit      -> "unit"
    | TC_string    -> "string"
    | TC_bytes     -> "bytes"
    | TC_nat       -> "nat"
    | TC_int       -> "int"
    | TC_mutez     -> "mutez"
    | TC_bool      -> "bool"
    | TC_operation -> "operation"
    | TC_address   -> "address"
    | TC_key       -> "key"
    | TC_key_hash  -> "key_hash"
    | TC_signature -> "signature"
    | TC_timestamp -> "timestamp"
    | TC_chain_id  -> "chain_id"
    in
  fprintf ppf "%s" s


and type_operator : type a . (formatter -> a -> unit) -> formatter -> a type_operator -> unit =
  fun f ppf to_ ->
  let s = match to_ with
    | TC_option (tv) -> Format.asprintf "option(%a)" f tv
    | TC_list   (tv) -> Format.asprintf "list(%a)" f tv
    | TC_set    (tv) -> Format.asprintf "set(%a)" f tv
    | TC_map    (k, v) -> Format.asprintf "Map (%a,%a)" f k f v
    | TC_big_map (k, v) -> Format.asprintf "Big Map (%a,%a)" f k f v
    | TC_contract (c) -> Format.asprintf "Contract (%a)" f c
    | TC_arrow (a , b) -> Format.asprintf "TC_Arrow (%a,%a)" f a f b
    in
  fprintf ppf "(TO_%s)" s

let literal ppf (l:literal) = match l with
  | Literal_unit -> fprintf ppf "Unit"
  | Literal_bool b -> fprintf ppf "%b" b
  | Literal_int n -> fprintf ppf "%d" n
  | Literal_nat n -> fprintf ppf "+%d" n
  | Literal_timestamp n -> fprintf ppf "+%d" n
  | Literal_mutez n -> fprintf ppf "%dmutez" n
  | Literal_string s -> fprintf ppf "%S" s
  | Literal_bytes b -> fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_address s -> fprintf ppf "address %S" s
  | Literal_operation _ -> fprintf ppf "Operation(...bytes)"
  | Literal_key s      -> fprintf ppf "key %s" s
  | Literal_key_hash s -> fprintf ppf "key_hash %s" s
  | Literal_signature s -> fprintf ppf "signature %s" s
  | Literal_chain_id s -> fprintf ppf "chain_id %s" s

let%expect_test _ =
  Format.printf "%a" literal (Literal_bytes (Bytes.of_string "foo")) ;
  [%expect{| 0x666f6f |}]
