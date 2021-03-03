open Stage_common.Enums

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
  | C_LIST_HEAD_OPT           ->  64
  | C_LIST_TAIL_OPT           ->  65
  (* Maps *)
  | C_MAP                     ->  66
  | C_MAP_EMPTY               ->  67
  | C_MAP_LITERAL             ->  68
  | C_MAP_GET                 ->  69
  | C_MAP_GET_FORCE           ->  70
  | C_MAP_ADD                 ->  71
  | C_MAP_REMOVE              ->  72
  | C_MAP_UPDATE              ->  73
  | C_MAP_ITER                ->  74
  | C_MAP_MAP                 ->  75
  | C_MAP_FOLD                ->  76
  | C_MAP_MEM                 ->  77
  | C_MAP_FIND                ->  78
  | C_MAP_FIND_OPT            ->  79
  (* Big Maps *)
  | C_BIG_MAP                 ->  80
  | C_BIG_MAP_EMPTY           ->  81
  | C_BIG_MAP_LITERAL         ->  82
  (* Crypto *)
  | C_SHA256                  ->  83
  | C_SHA512                  ->  84
  | C_BLAKE2b                 ->  85
  | C_HASH                    ->  86
  | C_HASH_KEY                ->  87
  | C_CHECK_SIGNATURE         ->  88
  | C_CHAIN_ID                ->  89
  (* Blockchain *)
  | C_CALL                    ->  90
  | C_CONTRACT                ->  91
  | C_CONTRACT_OPT            ->  92
  | C_CONTRACT_ENTRYPOINT     ->  93
  | C_CONTRACT_ENTRYPOINT_OPT ->  94
  | C_AMOUNT                  ->  95
  | C_BALANCE                 ->  96
  | C_SOURCE                  ->  97
  | C_SENDER                  ->  98
  | C_ADDRESS                 ->  99
  | C_SELF                    -> 100
  | C_SELF_ADDRESS            -> 101
  | C_IMPLICIT_ACCOUNT        -> 102
  | C_SET_DELEGATE            -> 103
  | C_CREATE_CONTRACT         -> 104
  | C_CONVERT_TO_LEFT_COMB    -> 105
  | C_CONVERT_TO_RIGHT_COMB   -> 106
  | C_CONVERT_FROM_LEFT_COMB  -> 107
  | C_CONVERT_FROM_RIGHT_COMB -> 108
  (* and more *)
  | C_TRUE                    -> 109
  | C_FALSE                   -> 110
  | C_TEST_ORIGINATE          -> 111
  | C_TEST_SET_NOW            -> 112
  | C_TEST_SET_SOURCE         -> 113
  | C_TEST_SET_BALANCE        -> 114
  | C_TEST_EXTERNAL_CALL      -> 115
  | C_TEST_GET_STORAGE        -> 116
  | C_TEST_GET_BALANCE        -> 117
  | C_TEST_ASSERT_FAILURE     -> 118
  | C_TEST_LOG                -> 119
  | C_SHA3                    -> 120
  | C_KECCAK                  -> 121
  | C_LEVEL                   -> 122
  | C_VOTING_POWER            -> 123
  | C_TOTAL_VOTING_POWER      -> 124
  | C_TICKET                  -> 125
  | C_READ_TICKET             -> 126
  | C_SPLIT_TICKET            -> 127
  | C_JOIN_TICKET             -> 128
  | C_PAIRING_CHECK           -> 129
  | C_MAP_GET_AND_UPDATE      -> 130
  | C_BIG_MAP_GET_AND_UPDATE  -> 131
  | C_SAPLING_EMPTY_STATE     -> 132
  | C_SAPLING_VERIFY_UPDATE   -> 133
  | C_SET_UPDATE              -> 134

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
