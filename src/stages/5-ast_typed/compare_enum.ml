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
  | C_SET_UPDATE              ->  58
  | C_SET_ITER                ->  59
  | C_SET_FOLD                ->  60
  | C_SET_FOLD_DESC           ->  61
  | C_SET_MEM                 ->  62
  (* List *)
  | C_LIST_EMPTY              ->  63
  | C_LIST_LITERAL            ->  64
  | C_LIST_ITER               ->  65
  | C_LIST_MAP                ->  66
  | C_LIST_FOLD               ->  67
  | C_LIST_FOLD_LEFT          ->  68
  | C_LIST_FOLD_RIGHT         ->  69
  | C_LIST_HEAD_OPT           ->  70
  | C_LIST_TAIL_OPT           ->  71
  (* Maps *)
  | C_MAP                     ->  72
  | C_MAP_EMPTY               ->  73
  | C_MAP_LITERAL             ->  74
  | C_MAP_GET                 ->  75
  | C_MAP_GET_FORCE           ->  76
  | C_MAP_ADD                 ->  77
  | C_MAP_REMOVE              ->  78
  | C_MAP_UPDATE              ->  79
  | C_MAP_ITER                ->  80
  | C_MAP_MAP                 ->  81
  | C_MAP_FOLD                ->  82
  | C_MAP_MEM                 ->  83
  | C_MAP_FIND                ->  84
  | C_MAP_FIND_OPT            ->  85
  (* Big Maps *)
  | C_BIG_MAP                 ->  86
  | C_BIG_MAP_EMPTY           ->  87
  | C_BIG_MAP_LITERAL         ->  88
  (* Crypto *)
  | C_SHA256                  ->  89
  | C_SHA512                  ->  90
  | C_BLAKE2b                 ->  91
  | C_HASH                    ->  92
  | C_HASH_KEY                ->  93
  | C_CHECK_SIGNATURE         ->  94
  | C_CHAIN_ID                ->  95
  (* Blockchain *)
  | C_CALL                    ->  96
  | C_CONTRACT                ->  97
  | C_CONTRACT_OPT            ->  98
  | C_CONTRACT_ENTRYPOINT     ->  99
  | C_CONTRACT_ENTRYPOINT_OPT -> 100
  | C_AMOUNT                  -> 101
  | C_BALANCE                 -> 102
  | C_SOURCE                  -> 103
  | C_SENDER                  -> 104
  | C_ADDRESS                 -> 105
  | C_SELF                    -> 106
  | C_SELF_ADDRESS            -> 107
  | C_IMPLICIT_ACCOUNT        -> 108
  | C_SET_DELEGATE            -> 109
  | C_CREATE_CONTRACT         -> 110
  | C_TRUE                    -> 115
  | C_FALSE                   -> 116
  | C_TEST_ORIGINATE          -> 117
  | C_TEST_SET_NOW            -> 118
  | C_TEST_SET_SOURCE         -> 119
  | C_TEST_SET_BAKER          -> 120
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> 121
  | C_TEST_GET_STORAGE        -> 122
  | C_TEST_GET_BALANCE        -> 123
  | C_TEST_MICHELSON_EQUAL    -> 124
  | C_TEST_LOG                -> 125
  | C_SHA3                    -> 126
  | C_KECCAK                  -> 127
  | C_LEVEL                   -> 128
  | C_VOTING_POWER            -> 129
  | C_TOTAL_VOTING_POWER      -> 130
  | C_TICKET                  -> 131
  | C_READ_TICKET             -> 132
  | C_SPLIT_TICKET            -> 133
  | C_JOIN_TICKET             -> 134
  | C_PAIRING_CHECK           -> 135
  | C_MAP_GET_AND_UPDATE      -> 136
  | C_BIG_MAP_GET_AND_UPDATE  -> 137
  | C_SAPLING_EMPTY_STATE     -> 138
  | C_SAPLING_VERIFY_UPDATE   -> 139
  | C_TEST_STATE_RESET        -> 142
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN -> 143
  | C_TEST_GET_NTH_BS         -> 144
  | C_TEST_LAST_ORIGINATIONS  -> 145
  | C_TEST_COMPILE_META_VALUE -> 146
  | C_TEST_RUN                -> 148
  | C_TEST_EVAL               -> 149
  | C_TEST_COMPILE_CONTRACT   -> 150
  | C_TEST_TO_CONTRACT        -> 151
  | C_TEST_ORIGINATE_FROM_FILE -> 152
  | C_TEST_GET_STORAGE_OF_ADDRESS -> 153
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> 154
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN -> 155
  | C_TEST_TO_ENTRYPOINT        -> 156
  | C_POLYMORPHIC_ADD         -> 157
  | C_NEVER                   -> 159
  | C_TEST_MUTATE_COUNT       -> 161
  | C_TEST_MUTATE_VALUE       -> 162
  | C_TEST_MUTATION_TEST      -> 163
  | C_TEST_MUTATION_TEST_ALL  -> 164
  | C_TEST_BOOTSTRAP_CONTRACT -> 165
  | C_TEST_NTH_BOOTSTRAP_CONTRACT -> 166
  | C_TEST_TO_TYPED_ADDRESS   -> 167
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS -> 168
  | C_TEST_SAVE_MUTATION      -> 169
  | C_TEST_SET_BIG_MAP        -> 170

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
