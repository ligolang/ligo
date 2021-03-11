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
  | C_FOLD_LEFT               ->  21
  | C_FOLD_RIGHT              ->  22
  (* MATH *)
  | C_NEG                     ->  23
  | C_ABS                     ->  24
  | C_ADD                     ->  25
  | C_SUB                     ->  26
  | C_MUL                     ->  27
  | C_EDIV                    ->  28
  | C_DIV                     ->  29
  | C_MOD                     ->  30
  (* LOGIC *)
  | C_NOT                     ->  31
  | C_AND                     ->  32
  | C_OR                      ->  33
  | C_XOR                     ->  34
  | C_LSL                     ->  35
  | C_LSR                     ->  36
  (* COMPARATOR *)
  | C_EQ                      ->  37
  | C_NEQ                     ->  38
  | C_LT                      ->  39
  | C_GT                      ->  40
  | C_LE                      ->  41
  | C_GE                      ->  42
  (* Bytes/ String *)
  | C_SIZE                    ->  43
  | C_CONCAT                  ->  44
  | C_SLICE                   ->  45
  | C_BYTES_PACK              ->  46
  | C_BYTES_UNPACK            ->  47
  | C_CONS                    ->  48
  (* Pair *)
  | C_PAIR                    ->  49
  | C_CAR                     ->  50
  | C_CDR                     ->  51
  | C_LEFT                    ->  52
  | C_RIGHT                   ->  53
  (* Set *)
  | C_SET_EMPTY               ->  54
  | C_SET_LITERAL             ->  55
  | C_SET_ADD                 ->  56
  | C_SET_REMOVE              ->  57
  | C_SET_ITER                ->  58
  | C_SET_FOLD                ->  59
  | C_SET_FOLD_DESC          ->  60
  | C_SET_MEM                 ->  61
  (* List *)
  | C_LIST_EMPTY              ->  60
  | C_LIST_LITERAL            ->  61
  | C_LIST_ITER               ->  62
  | C_LIST_MAP                ->  63
  | C_LIST_FOLD               ->  64
  | C_LIST_FOLD_LEFT          ->  65
  | C_LIST_FOLD_RIGHT         ->  66
  | C_LIST_HEAD_OPT           ->  67
  | C_LIST_TAIL_OPT           ->  68
  (* Maps *)
  | C_MAP                     ->  69
  | C_MAP_EMPTY               ->  70
  | C_MAP_LITERAL             ->  71
  | C_MAP_GET                 ->  72
  | C_MAP_GET_FORCE           ->  73
  | C_MAP_ADD                 ->  74
  | C_MAP_REMOVE              ->  75
  | C_MAP_UPDATE              ->  76
  | C_MAP_ITER                ->  77
  | C_MAP_MAP                 ->  78
  | C_MAP_FOLD                ->  79
  | C_MAP_MEM                 ->  80
  | C_MAP_FIND                ->  81
  | C_MAP_FIND_OPT            ->  82
  (* Big Maps *)
  | C_BIG_MAP                 ->  83
  | C_BIG_MAP_EMPTY           ->  84
  | C_BIG_MAP_LITERAL         ->  85
  (* Crypto *)
  | C_SHA256                  ->  86
  | C_SHA512                  ->  87
  | C_BLAKE2b                 ->  88
  | C_HASH                    ->  89
  | C_HASH_KEY                ->  90
  | C_CHECK_SIGNATURE         ->  91
  | C_CHAIN_ID                ->  92
  (* Blockchain *)
  | C_CALL                    ->  93
  | C_CONTRACT                ->  94
  | C_CONTRACT_OPT            ->  95
  | C_CONTRACT_ENTRYPOINT     ->  96
  | C_CONTRACT_ENTRYPOINT_OPT ->  97
  | C_AMOUNT                  ->  98
  | C_BALANCE                 ->  99
  | C_SOURCE                  -> 100
  | C_SENDER                  -> 101
  | C_ADDRESS                 -> 102
  | C_SELF                    -> 103
  | C_SELF_ADDRESS            -> 104
  | C_IMPLICIT_ACCOUNT        -> 105
  | C_SET_DELEGATE            -> 106
  | C_CREATE_CONTRACT         -> 107
  | C_CONVERT_TO_LEFT_COMB    -> 108
  | C_CONVERT_TO_RIGHT_COMB   -> 109
  | C_CONVERT_FROM_LEFT_COMB  -> 110
  | C_CONVERT_FROM_RIGHT_COMB -> 111
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
