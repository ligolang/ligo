open Stage_common.Enums

let type_constant_tag = function
  | TC_unit                      ->  1
  | TC_string                    ->  2
  | TC_bytes                     ->  3
  | TC_nat                       ->  4
  | TC_int                       ->  5
  | TC_mutez                     ->  6
  | TC_operation                 ->  7
  | TC_address                   ->  8
  | TC_key                       ->  9
  | TC_key_hash                  -> 10
  | TC_chain_id                  -> 11
  | TC_signature                 -> 12
  | TC_timestamp                 -> 13
  | TC_contract                  -> 14
  | TC_option                    -> 15
  | TC_list                      -> 16
  | TC_set                       -> 17
  | TC_map                       -> 18
  | TC_big_map                   -> 19
  | TC_map_or_big_map            -> 20
  | TC_michelson_pair            -> 21
  | TC_michelson_or              -> 22
  | TC_michelson_pair_right_comb -> 23
  | TC_michelson_pair_left_comb  -> 24
  | TC_michelson_or_right_comb   -> 25
  | TC_michelson_or_left_comb    -> 26

let type_constant a b = Int.compare (type_constant_tag a) (type_constant_tag b)

let constant'_tag = function
  | C_INT                     ->   1
  | C_UNIT                    ->   2
  | C_NIL                     ->   3
  | C_NOW                     ->   4
  | C_IS_NAT                  ->   5
  | C_SOME                    ->   6
  | C_NONE                    ->   7
  | C_ASSERTION               ->   8
  | C_ASSERT_INFERRED         ->   9
  | C_ASSERT_SOME             ->  10
  | C_FAILWITH                ->  11
  | C_UPDATE                  ->  12
  (* Loops *)
  | C_ITER                    ->  13
  | C_FOLD_WHILE              ->  14
  | C_FOLD_CONTINUE           ->  15
  | C_FOLD_STOP               ->  16
  | C_LOOP_LEFT               ->  17
  | C_LOOP_CONTINUE           ->  18
  | C_LOOP_STOP               ->  19
  | C_FOLD                    ->  20
  (* MATH *)
  | C_NEG                     ->  21
  | C_ABS                     ->  22
  | C_ADD                     ->  23
  | C_SUB                     ->  24
  | C_MUL                     ->  25
  | C_EDIV                    ->  26
  | C_DIV                     ->  27
  | C_MOD                     ->  28
  (* LOGIC *)
  | C_NOT                     ->  29
  | C_AND                     ->  30
  | C_OR                      ->  31
  | C_XOR                     ->  32
  | C_LSL                     ->  33
  | C_LSR                     ->  34
  (* COMPARATOR *)
  | C_EQ                      ->  35
  | C_NEQ                     ->  36
  | C_LT                      ->  37
  | C_GT                      ->  38
  | C_LE                      ->  39
  | C_GE                      ->  40
  (* Bytes/ String *)
  | C_SIZE                    ->  41
  | C_CONCAT                  ->  42
  | C_SLICE                   ->  43
  | C_BYTES_PACK              ->  44
  | C_BYTES_UNPACK            ->  45
  | C_CONS                    ->  46
  (* Pair *)
  | C_PAIR                    ->  47
  | C_CAR                     ->  48
  | C_CDR                     ->  49
  | C_LEFT                    ->  50
  | C_RIGHT                   ->  51
  (* Set *)
  | C_SET_EMPTY               ->  52
  | C_SET_LITERAL             ->  53
  | C_SET_ADD                 ->  54
  | C_SET_REMOVE              ->  55
  | C_SET_ITER                ->  56
  | C_SET_FOLD                ->  57
  | C_SET_MEM                 ->  58
  (* List *)
  | C_LIST_EMPTY              ->  59
  | C_LIST_LITERAL            ->  60
  | C_LIST_ITER               ->  61
  | C_LIST_MAP                ->  62
  | C_LIST_FOLD               ->  63
  (* Maps *)
  | C_MAP                     ->  64
  | C_MAP_EMPTY               ->  65
  | C_MAP_LITERAL             ->  66
  | C_MAP_GET                 ->  67
  | C_MAP_GET_FORCE           ->  68
  | C_MAP_ADD                 ->  69
  | C_MAP_REMOVE              ->  70
  | C_MAP_UPDATE              ->  71
  | C_MAP_ITER                ->  72
  | C_MAP_MAP                 ->  73
  | C_MAP_FOLD                ->  74
  | C_MAP_MEM                 ->  75
  | C_MAP_FIND                ->  76
  | C_MAP_FIND_OPT            ->  77
  (* Big Maps *)
  | C_BIG_MAP                 ->  78
  | C_BIG_MAP_EMPTY           ->  79
  | C_BIG_MAP_LITERAL         ->  80
  (* Crypto *)
  | C_SHA256                  ->  81
  | C_SHA512                  ->  82
  | C_BLAKE2b                 ->  83
  | C_HASH                    ->  84
  | C_HASH_KEY                ->  85
  | C_CHECK_SIGNATURE         ->  86
  | C_CHAIN_ID                ->  87
  (* Blockchain *)
  | C_CALL                    ->  88
  | C_CONTRACT                ->  89
  | C_CONTRACT_OPT            ->  90
  | C_CONTRACT_ENTRYPOINT     ->  91
  | C_CONTRACT_ENTRYPOINT_OPT ->  92
  | C_AMOUNT                  ->  93
  | C_BALANCE                 ->  94
  | C_SOURCE                  ->  95
  | C_SENDER                  ->  96
  | C_ADDRESS                 ->  97
  | C_SELF                    ->  98
  | C_SELF_ADDRESS            ->  99
  | C_IMPLICIT_ACCOUNT        -> 100
  | C_SET_DELEGATE            -> 101
  | C_CREATE_CONTRACT         -> 102
  | C_CONVERT_TO_LEFT_COMB    -> 103
  | C_CONVERT_TO_RIGHT_COMB   -> 104
  | C_CONVERT_FROM_LEFT_COMB  -> 105
  | C_CONVERT_FROM_RIGHT_COMB -> 106

let constant' a b = Int.compare (constant'_tag a) (constant'_tag b)

let literal_tag = function
  | Literal_unit        ->  1
  | Literal_int _       ->  2
  | Literal_nat _       ->  3
  | Literal_timestamp _ ->  4
  | Literal_mutez _     ->  5
  | Literal_string _    ->  6
  | Literal_bytes _     ->  7
  | Literal_address _   ->  8
  | Literal_signature _ ->  9
  | Literal_key _       -> 10
  | Literal_key_hash _  -> 11
  | Literal_chain_id _  -> 12
  | Literal_operation _ -> 13

let literal a b = Int.compare (literal_tag a) (literal_tag b)
