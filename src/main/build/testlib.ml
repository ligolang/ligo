let lib (s : Syntax_types.t) =
  match s with
  | PascaLIGO _ | ReasonLIGO | JsLIGO ->"
type test_exec_error_balance_too_low = { contract_too_low : address ; contract_balance : tez ; spend_request : tez }
type test_exec_error = Rejected of michelson_program * address
                     | Balance_too_low of test_exec_error_balance_too_low
                     | Other of string
type test_exec_result = Success of nat | Fail of test_exec_error
module Test = struct
  [@hidden] [@inline] let to_contract (type p s) (t : (p, s) typed_address) : p contract = [%external \"TEST_TO_CONTRACT\"] t
  [@hidden] [@inline] let originate_from_file ((fn, e, v, s, t) : string * string * string list * michelson_program * tez) : address * michelson_program * int = [%external \"TEST_ORIGINATE_FROM_FILE\"] fn e v s t
  [@hidden] [@inline] let originate (type p s) ((f, s, t) : (p * s -> operation list * s) * s * tez) : ((p, s) typed_address * michelson_program * int) = [%external \"TEST_ORIGINATE\"] f s t
  [@hidden] [@inline] let set_source (a : address) : unit = [%external \"TEST_SET_SOURCE\"] a
  [@hidden] [@inline] let set_baker (a : address) : unit = [%external \"TEST_SET_BAKER\"] a
  [@hidden] [@inline] let transfer ((a, s, t) : address * michelson_program * tez) : test_exec_result = [%external \"TEST_EXTERNAL_CALL_TO_ADDRESS\"] a s t
  [@hidden] [@inline] let transfer_exn ((a, s, t) : address * michelson_program * tez) : nat = [%external \"TEST_EXTERNAL_CALL_TO_ADDRESS_EXN\"] a s t
  [@hidden] [@inline] let transfer_to_contract (type p) ((a, s, t) : p contract * p * tez) : test_exec_result = [%external \"TEST_EXTERNAL_CALL_TO_CONTRACT\"] a s t
  [@hidden] [@inline] let transfer_to_contract_exn (type p) ((a, s, t) : p contract * p * tez) : nat = [%external \"TEST_EXTERNAL_CALL_TO_CONTRACT_EXN\"] a s t
  [@hidden] [@inline] let get_storage (type a b) (t : (a, b) typed_address) : b = [%external \"TEST_GET_STORAGE\"] t
  [@hidden] [@inline] let get_storage_of_address (a : address) : michelson_program = [%external \"TEST_GET_STORAGE_OF_ADDRESS\"] a
  [@hidden] [@inline] let get_balance (a : address) : tez = [%external \"TEST_GET_BALANCE\"] a
  [@hidden] [@inline] let michelson_equal ((m1, m2) : michelson_program * michelson_program) : bool = [%external \"TEST_MICHELSON_EQUAL\"] m1 m2
  [@hidden] [@inline] let log (type a) (v : a) : unit = [%external \"TEST_LOG\"] v
  [@hidden] [@inline] let reset_state ((n, l) : nat * tez list) : unit = [%external \"TEST_STATE_RESET\"] n l
  [@hidden] [@inline] let get_voting_power (kh : key_hash) : nat = [%external \"TEST_GET_VOTING_POWER\"] kh
  [@thunk] [@hidden] [@inline] let get_total_voting_power : nat = [%external \"TEST_GET_TOTAL_VOTING_POWER\"]
  [@hidden] [@inline] let bootstrap_contract (type p s) ((f, s, t) : (p * s -> operation list * s) * s * tez) : unit = [%external \"TEST_BOOTSTRAP_CONTRACT\"] f s t
  [@hidden] [@inline] let nth_bootstrap_contract (i : nat) : address = [%external \"TEST_NTH_BOOTSTRAP_CONTRACT\"] i
  [@hidden] [@inline] let nth_bootstrap_account (i : int) : address = [%external \"TEST_GET_NTH_BS\"] i
  [@hidden] [@inline] let nth_bootstrap_typed_address (type a b) (n : nat) : (a, b) typed_address = [%external \"TEST_NTH_BOOTSTRAP_TYPED_ADDRESS\"] n
  [@hidden] [@inline] let last_originations (u : unit) : (address, address list) map = [%external \"TEST_LAST_ORIGINATIONS\"] u
  [@hidden] [@inline] let compile_value (type a) (v : a) : michelson_program = [%external \"TEST_COMPILE_META_VALUE\"] v
  [@hidden] [@inline] let mutate_value (type a) ((n, v) : nat * a) : (a * mutation) option = [%external \"TEST_MUTATE_VALUE\"] n v
  [@hidden] [@inline] let save_mutation ((s, m) : string * mutation) : string option = [%external \"TEST_SAVE_MUTATION\"] s m
  [@hidden] [@inline] let mutation_test (type a b) ((v, f) : a * (a -> b)) : (b * mutation) option = [%external \"TEST_MUTATION_TEST\"] v f
  [@hidden] [@inline] let mutation_test_all (type a b) ((v, f) : a * (a -> b)) : (b * mutation) list = [%external \"TEST_MUTATION_TEST_ALL\"] v f
  [@hidden] [@inline] let run (type a b) ((f, v) : (a -> b) * a) : michelson_program = [%external \"TEST_RUN\"] f v
  [@hidden] [@inline] let eval (type a) (v : a) : michelson_program = [%external \"TEST_EVAL\"] v
  [@hidden] [@inline] let decompile (type a) (m : michelson_program) : a = [%external \"TEST_DECOMPILE\"] m
  [@hidden] [@inline] let random (type a) (u : unit) : a = [%external \"TEST_RANDOM\"] u
  [@hidden] [@inline] let add_account ((s, k) : string * key) : unit = [%external \"TEST_ADD_ACCOUNT\"] s k
  [@hidden] [@inline] let new_account (u : unit) : string * key = [%external \"TEST_NEW_ACCOUNT\"] u
  [@hidden] [@inline] let baker_account ((p, o) : (string * key) * tez option) : unit = [%external \"TEST_BAKER_ACCOUNT\"] p o
  [@hidden] [@inline] let bake_until_n_cycle_end (n : nat) : unit = [%external \"TEST_BAKE_UNTIL_N_CYCLE_END\"] n
  [@hidden] [@inline] let register_delegate (kh : key_hash) : unit = [%external \"TEST_REGISTER_DELEGATE\"] kh
  [@hidden] [@inline] let register_constant (m : michelson_program) : string = [%external \"TEST_REGISTER_CONSTANT\"] m
  [@hidden] [@inline] let cast_address (type a b) (a : address) : (a, b) typed_address = [%external \"TEST_CAST_ADDRESS\"] a
  [@hidden] [@inline] let to_typed_address (type a b) (c : a contract) : (a, b) typed_address = [%external \"TEST_TO_TYPED_ADDRESS\"] c
  [@hidden] [@inline] let to_entrypoint (type a b c) ((s, t) : string * (a, b) typed_address) : c contract = [%external \"TEST_TO_ENTRYPOINT\"] s t
  [@hidden] [@inline] let set_big_map (type a b) ((i, m) : int * (a, b) big_map) : unit = [%external \"TEST_SET_BIG_MAP\"] i m
  [@hidden] [@inline] let create_chest ((b, n) : bytes * nat) : chest * chest_key = [%external \"TEST_CREATE_CHEST\"] b n
  [@hidden] [@inline] let create_chest_key ((c, n) : chest * nat) : chest_key = [%external \"TEST_CREATE_CHEST_KEY\"] c n
  [@hidden] [@inline] let constant_to_michelson_program (s : string) : michelson_program = [%external \"TEST_CONSTANT_TO_MICHELSON\"] s
  [@hidden] [@inline] let restore_context (u : unit) : unit = [%external \"TEST_POP_CONTEXT\"] u
  [@hidden] [@inline] let save_context (u : unit) : unit = [%external \"TEST_PUSH_CONTEXT\"] u
end
"
 | CameLIGO -> "
type test_exec_error_balance_too_low = { contract_too_low : address ; contract_balance : tez ; spend_request : tez }
type test_exec_error = Rejected of michelson_program * address
                     | Balance_too_low of test_exec_error_balance_too_low
                     | Other of string
type test_exec_result = Success of nat | Fail of test_exec_error
module Test = struct
  [@hidden] [@inline] let to_contract (type p s) (t : (p, s) typed_address) : p contract = [%external \"TEST_TO_CONTRACT\"] t
  [@hidden] [@inline] let originate_from_file (fn : string) (e : string) (v : string list) (s : michelson_program)  (t : tez) : address * michelson_program * int = [%external \"TEST_ORIGINATE_FROM_FILE\"] fn e v s t
  [@hidden] [@inline] let originate (type p s) (f : p * s -> operation list * s) (s : s) (t : tez) : ((p, s) typed_address * michelson_program * int) = [%external \"TEST_ORIGINATE\"] f s t
  [@hidden] [@inline] let set_source (a : address) : unit = [%external \"TEST_SET_SOURCE\"] a
  [@hidden] [@inline] let set_baker (a : address) : unit = [%external \"TEST_SET_BAKER\"] a
  [@hidden] [@inline] let transfer (a : address) (s : michelson_program) (t : tez) : test_exec_result = [%external \"TEST_EXTERNAL_CALL_TO_ADDRESS\"] a s t
  [@hidden] [@inline] let transfer_exn (a : address) (s : michelson_program) (t : tez) : nat = [%external \"TEST_EXTERNAL_CALL_TO_ADDRESS_EXN\"] a s t
  [@hidden] [@inline] let transfer_to_contract (type p) (a : p contract) (s : p) (t : tez) : test_exec_result = [%external \"TEST_EXTERNAL_CALL_TO_CONTRACT\"] a s t
  [@hidden] [@inline] let transfer_to_contract_exn (type p) (a : p contract) (s : p) (t : tez) : nat = [%external \"TEST_EXTERNAL_CALL_TO_CONTRACT_EXN\"] a s t
  [@hidden] [@inline] let get_storage (type a b) (t : (a, b) typed_address) : b = [%external \"TEST_GET_STORAGE\"] t
  [@hidden] [@inline] let get_storage_of_address (a : address) : michelson_program = [%external \"TEST_GET_STORAGE_OF_ADDRESS\"] a
  [@hidden] [@inline] let get_balance (a : address) : tez = [%external \"TEST_GET_BALANCE\"] a
  [@hidden] [@inline] let michelson_equal (m1 : michelson_program) (m2 : michelson_program) : bool = [%external \"TEST_MICHELSON_EQUAL\"] m1 m2
  [@hidden] [@inline] let log (type a) (v : a) : unit = [%external \"TEST_LOG\"] v
  [@hidden] [@inline] let reset_state (n : nat) (l : tez list) : unit = [%external \"TEST_STATE_RESET\"] n l
  [@hidden] [@inline] let get_voting_power (kh : key_hash) : nat = [%external \"TEST_GET_VOTING_POWER\"] kh
  [@thunk] [@hidden] [@inline] let get_total_voting_power : nat = [%external \"TEST_GET_TOTAL_VOTING_POWER\"]
  [@hidden] [@inline] let bootstrap_contract (type p s) (f : p * s -> operation list * s) (s : s) (t : tez) : unit = [%external \"TEST_BOOTSTRAP_CONTRACT\"] f s t
  [@hidden] [@inline] let nth_bootstrap_contract (i : nat) : address = [%external \"TEST_NTH_BOOTSTRAP_CONTRACT\"] i
  [@hidden] [@inline] let nth_bootstrap_account (i : int) : address = [%external \"TEST_GET_NTH_BS\"] i
  [@hidden] [@inline] let nth_bootstrap_typed_address (type a b) (n : nat) : (a, b) typed_address = [%external \"TEST_NTH_BOOTSTRAP_TYPED_ADDRESS\"] n
  [@hidden] [@inline] let last_originations (u : unit) : (address, address list) map = [%external \"TEST_LAST_ORIGINATIONS\"] u
  [@hidden] [@inline] let compile_value (type a) (v : a) : michelson_program = [%external \"TEST_COMPILE_META_VALUE\"] v
  [@hidden] [@inline] let mutate_value (type a) (n : nat) (v : a) : (a * mutation) option = [%external \"TEST_MUTATE_VALUE\"] n v
  [@hidden] [@inline] let save_mutation (s : string) (m : mutation) : string option = [%external \"TEST_SAVE_MUTATION\"] s m
  [@hidden] [@inline] let mutation_test (type a b) (v : a) (f : a -> b) : (b * mutation) option = [%external \"TEST_MUTATION_TEST\"] v f
  [@hidden] [@inline] let mutation_test_all (type a b) (v : a) (f : a -> b) : (b * mutation) list = [%external \"TEST_MUTATION_TEST_ALL\"] v f
  [@hidden] [@inline] let run (type a b) (f : a -> b) (v : a) : michelson_program = [%external \"TEST_RUN\"] f v
  [@hidden] [@inline] let eval (type a) (v : a) : michelson_program = [%external \"TEST_EVAL\"] v
  [@hidden] [@inline] let decompile (type a) (m : michelson_program) : a = [%external \"TEST_DECOMPILE\"] m
  [@hidden] [@inline] let random (type a) (u : unit) : a = [%external \"TEST_RANDOM\"] u
  [@hidden] [@inline] let add_account (s : string) (k : key) : unit = [%external \"TEST_ADD_ACCOUNT\"] s k
  [@hidden] [@inline] let new_account (u : unit) : string * key = [%external \"TEST_NEW_ACCOUNT\"] u
  [@hidden] [@inline] let baker_account (p : string * key) (o : tez option) : unit = [%external \"TEST_BAKER_ACCOUNT\"] p o
  [@hidden] [@inline] let bake_until_n_cycle_end (n : nat) : unit = [%external \"TEST_BAKE_UNTIL_N_CYCLE_END\"] n
  [@hidden] [@inline] let register_delegate (kh : key_hash) : unit = [%external \"TEST_REGISTER_DELEGATE\"] kh
  [@hidden] [@inline] let register_constant (m : michelson_program) : string = [%external \"TEST_REGISTER_CONSTANT\"] m
  [@hidden] [@inline] let cast_address (type a b) (a : address) : (a, b) typed_address = [%external \"TEST_CAST_ADDRESS\"] a
  [@hidden] [@inline] let to_typed_address (type a b) (c : a contract) : (a, b) typed_address = [%external \"TEST_TO_TYPED_ADDRESS\"] c
  [@hidden] [@inline] let to_entrypoint (type a b c) (s : string) (t : (a, b) typed_address) : c contract = [%external \"TEST_TO_ENTRYPOINT\"] s t
  [@hidden] [@inline] let set_big_map (type a b) (i : int) (m : (a, b) big_map) : unit = [%external \"TEST_SET_BIG_MAP\"] i m
  [@hidden] [@inline] let create_chest (b : bytes) (n : nat) : chest * chest_key = [%external \"TEST_CREATE_CHEST\"] b n
  [@hidden] [@inline] let create_chest_key (c : chest) (n : nat) : chest_key = [%external \"TEST_CREATE_CHEST_KEY\"] c n
  [@hidden] [@inline] let constant_to_michelson_program (s : string) : michelson_program = [%external \"TEST_CONSTANT_TO_MICHELSON\"] s
  [@hidden] [@inline] let restore_context (u : unit) : unit = [%external \"TEST_POP_CONTEXT\"] u
  [@hidden] [@inline] let save_context (u : unit) : unit = [%external \"TEST_PUSH_CONTEXT\"] u
end
"

let lib_stub (s : Syntax_types.t) =
  match s with
  | PascaLIGO _ | ReasonLIGO | JsLIGO ->"
type michelson_program = unit
type test_exec_error_balance_too_low = { contract_too_low : address ; contract_balance : tez ; spend_request : tez }
type test_exec_error = Rejected of michelson_program * address
                     | Balance_too_low of test_exec_error_balance_too_low
                     | Other of string
type test_exec_result = Success of nat | Fail of test_exec_error
module Test = struct
  [@private] [@hidden] [@inline] let failwith (type a) (v : a) : a external_failwith = [%external \"FAILWITH\"] v
  type ('a, 'b) typed_address = unit
  type michelson_program = unit
  type test_exec_result = unit
  type mutation = unit
  [@hidden] [@inline] let to_contract (type p s) (_t : (p, s) typed_address) : p contract = failwith \"TEST MODE\"
  [@hidden] [@inline] let originate_from_file ((_fn, _e, _v, _s, _t) : string * string * string list * michelson_program * tez) : address * michelson_program * int = failwith \"TEST MODE\"
  [@hidden] [@inline] let originate (type p s) ((_f, _s, _t) : (p * s -> operation list * s) * s * tez) : ((p, s) typed_address * michelson_program * int) = failwith \"TEST MODE\"
  [@hidden] [@inline] let set_source (_a : address) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let set_baker (_a : address) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let transfer ((_a, _s, _t) : address * michelson_program * tez) : test_exec_result = failwith \"TEST MODE\"
  [@hidden] [@inline] let transfer_exn ((_a, _s, _t) : address * michelson_program * tez) : nat = failwith \"TEST MODE\"
  [@hidden] [@inline] let transfer_to_contract (type p) ((_a, _s, _t) : p contract * p * tez) : test_exec_result = failwith \"TEST MODE\"
  [@hidden] [@inline] let transfer_to_contract_exn (type p) ((_a, _s, _t) : p contract * p * tez) : nat = failwith \"TEST MODE\"
  [@hidden] [@inline] let get_storage (type a b) (_t : (a, b) typed_address) : b = failwith \"TEST MODE\"
  [@hidden] [@inline] let get_storage_of_address (_a : address) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let get_balance (_a : address) : tez = failwith \"TEST MODE\"
  [@hidden] [@inline] let michelson_equal ((_m1, _m2) : michelson_program * michelson_program) : bool = failwith \"TEST MODE\"
  [@hidden] [@inline] let log (type a) (_v : a) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let reset_state ((_n, _l) : nat * tez list) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let get_voting_power (_kh : key_hash) : nat = failwith \"TEST MODE\"
  [@thunk] [@hidden] [@inline] let get_total_voting_power : nat = failwith \"TEST MODE\"
  [@hidden] [@inline] let bootstrap_contract (type p s) ((_f, _s, _t) : (p * s -> operation list * s) * s * tez) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let nth_bootstrap_contract (_i : nat) : address = failwith \"TEST MODE\"
  [@hidden] [@inline] let nth_bootstrap_account (_i : int) : address = failwith \"TEST MODE\"
  [@hidden] [@inline] let nth_bootstrap_typed_address (type a b) (_n : nat) : (a, b) typed_address = failwith \"TEST MODE\"
  [@hidden] [@inline] let last_originations (_u : unit) : (address, address list) map = failwith \"TEST MODE\"
  [@hidden] [@inline] let compile_value (type a) (_v : a) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let mutate_value (type a) ((_n, _v) : nat * a) : (a * mutation) option = failwith \"TEST MODE\"
  [@hidden] [@inline] let save_mutation ((_s, _m) : string * mutation) : string option = failwith \"TEST MODE\"
  [@hidden] [@inline] let mutation_test (type a b) ((_v, _f) : a * (a -> b)) : (b * mutation) option = failwith \"TEST MODE\"
  [@hidden] [@inline] let mutation_test_all (type a b) ((_v, _f) : a * (a -> b)) : (b * mutation) list = failwith \"TEST MODE\"
  [@hidden] [@inline] let run (type a b) ((_f, _v) : (a -> b) * a) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let eval (type a) (_v : a) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let decompile (type a) (_m : michelson_program) : a = failwith \"TEST MODE\"
  [@hidden] [@inline] let random (type a) (_u : unit) : a option = failwith \"TEST MODE\"
  [@hidden] [@inline] let add_account ((_s, _k) : string * key) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let new_account (_u : unit) : string * key = failwith \"TEST MODE\"
  [@hidden] [@inline] let baker_account ((_p, _o) : (string * key) * tez option) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let bake_until_n_cycle_end (_n : nat) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let register_delegate (_kh : key_hash) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let register_constant (_m : michelson_program) : string = failwith \"TEST MODE\"
  [@hidden] [@inline] let cast_address (type a b) (_a : address) : (a, b) typed_address = failwith \"TEST MODE\"
  [@hidden] [@inline] let to_typed_address (type a b) (_c : a contract) : (a, b) typed_address = failwith \"TEST MODE\"
  [@hidden] [@inline] let to_entrypoint (type a b c) ((_s, _t) : string * (a, b) typed_address) : c contract = failwith \"TEST MODE\"
  [@hidden] [@inline] let set_big_map (type a b) ((_i, _m) : int * (a, b) big_map) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let create_chest ((_b, _n) : bytes * nat) : chest * chest_key = failwith \"TEST MODE\"
  [@hidden] [@inline] let create_chest_key ((_c, _n) : chest * nat) : chest_key = failwith \"TEST MODE\"
  [@hidden] [@inline] let constant_to_michelson_program (_s : string) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let restore_context (_u : unit) : unit = failwith \"TEST_POP_CONTEXT\"
  [@hidden] [@inline] let save_context (_u : unit) : unit = failwith \"TEST_PUSH_CONTEXT\"
end
"
 | CameLIGO -> "
type michelson_program = unit
type test_exec_error_balance_too_low = { contract_too_low : address ; contract_balance : tez ; spend_request : tez }
type test_exec_error = Rejected of michelson_program * address
                     | Balance_too_low of test_exec_error_balance_too_low
                     | Other of string
type test_exec_result = Success of nat | Fail of test_exec_error
module Test = struct
  [@private] [@hidden] [@inline] let failwith (type a) (v : a) : a external_failwith = [%external \"FAILWITH\"] v
  type ('a, 'b) typed_address = unit
  type michelson_program = unit
  type test_exec_result = unit
  type mutation = unit
  [@hidden] [@inline] let to_contract (type p s) (_t : (p, s) typed_address) : p contract = failwith \"TEST MODE\"
  [@hidden] [@inline] let originate_from_file (_fn : string) (_e : string) (_v : string list) (_s : michelson_program)  (_t : tez) : address * michelson_program * int = failwith \"TEST MODE\"
  [@hidden] [@inline] let originate (type p s) (_f : p * s -> operation list * s) (_s : s) (_t : tez) : ((p, s) typed_address * michelson_program * int) = failwith \"TEST MODE\"
  [@hidden] [@inline] let set_source (_a : address) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let set_baker (_a : address) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let transfer (_a : address) (_s : michelson_program) (_t : tez) : test_exec_result = failwith \"TEST MODE\"
  [@hidden] [@inline] let transfer_exn (_a : address) (_s : michelson_program) (_t : tez) : nat = failwith \"TEST MODE\"
  [@hidden] [@inline] let transfer_to_contract (type p) (_a : p contract) (_s : p) (_t : tez) : test_exec_result = failwith \"TEST MODE\"
  [@hidden] [@inline] let transfer_to_contract_exn (type p) (_a : p contract) (_s : p) (_t : tez) : nat = failwith \"TEST MODE\"
  [@hidden] [@inline] let get_storage (type a b) (_t : (a, b) typed_address) : b = failwith \"TEST MODE\"
  [@hidden] [@inline] let get_storage_of_address (_a : address) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let get_balance (_a : address) : tez = failwith \"TEST MODE\"
  [@hidden] [@inline] let michelson_equal (_m1 : michelson_program) (_m2 : michelson_program) : bool = failwith \"TEST MODE\"
  [@hidden] [@inline] let log (type a) (_v : a) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let reset_state (_n : nat) (_l : tez list) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let get_voting_power (_kh : key_hash) : nat = failwith \"TEST MODE\"
  [@thunk] [@hidden] [@inline] let get_total_voting_power : nat = failwith \"TEST MODE\"
  [@hidden] [@inline] let bootstrap_contract (type p s) (_f : p * s -> operation list * s) (_s : s) (_t : tez) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let nth_bootstrap_contract (_i : nat) : address = failwith \"TEST MODE\"
  [@hidden] [@inline] let nth_bootstrap_account (_i : int) : address = failwith \"TEST MODE\"
  [@hidden] [@inline] let nth_bootstrap_typed_address (type a b) (_n : nat) : (a, b) typed_address = failwith \"TEST MODE\"
  [@hidden] [@inline] let last_originations (_u : unit) : (address, address list) map = failwith \"TEST MODE\"
  [@hidden] [@inline] let compile_value (type a) (_v : a) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let mutate_value (type a) (_n : nat) (_v : a) : (a * mutation) option = failwith \"TEST MODE\"
  [@hidden] [@inline] let save_mutation (_s : string) (_m : mutation) : string option = failwith \"TEST MODE\"
  [@hidden] [@inline] let mutation_test (type a b) (_v : a) (_f : a -> b) : (b * mutation) option = failwith \"TEST MODE\"
  [@hidden] [@inline] let mutation_test_all (type a b) (_v : a) (_f : a -> b) : (b * mutation) list = failwith \"TEST MODE\"
  [@hidden] [@inline] let run (type a b) (_f : a -> b) (_v : a) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let eval (type a) (_v : a) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let decompile (type a) (_m : michelson_program) : a = failwith \"TEST MODE\"
  [@hidden] [@inline] let random (type a) (_u : unit) : a option = failwith \"TEST MODE\"
  [@hidden] [@inline] let add_account (_s : string) (_k : key) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let new_account (_u : unit) : string * key = failwith \"TEST MODE\"
  [@hidden] [@inline] let baker_account (_p : string * key) (_o : tez option) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let bake_until_n_cycle_end (_n : nat) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let register_delegate (_kh : key_hash) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let register_constant (_m : michelson_program) : string = failwith \"TEST MODE\"
  [@hidden] [@inline] let cast_address (type a b) (_a : address) : (a, b) typed_address = failwith \"TEST MODE\"
  [@hidden] [@inline] let to_typed_address (type a b) (_c : a contract) : (a, b) typed_address = failwith \"TEST MODE\"
  [@hidden] [@inline] let to_entrypoint (type a b c) (_s : string) (_t : (a, b) typed_address) : c contract = failwith \"TEST MODE\"
  [@hidden] [@inline] let set_big_map (type a b) (_i : int) (_m : (a, b) big_map) : unit = failwith \"TEST MODE\"
  [@hidden] [@inline] let create_chest (_b : bytes) (_n : nat) : chest * chest_key = failwith \"TEST MODE\"
  [@hidden] [@inline] let create_chest_key (_c : chest) (_n : nat) : chest_key = failwith \"TEST MODE\"
  [@hidden] [@inline] let constant_to_michelson_program (_s : string) : michelson_program = failwith \"TEST MODE\"
  [@hidden] [@inline] let restore_context (_u : unit) : unit = failwith \"TEST_POP_CONTEXT\"
  [@hidden] [@inline] let save_context (_u : unit) : unit = failwith \"TEST_PUSH_CONTEXT\"
end
"

let testlib ~options syntax =
  if options.Compiler_options.middle_end.test then
    match Simple_utils.Trace.to_stdlib_result @@
            Ligo_compile.Utils.type_contract_string ~add_warning:(fun _ -> ()) ~options CameLIGO (lib syntax) with
    | Ok s -> s
    | Error e ->
       let error_msg = Format.asprintf "%a" (Main_errors.Formatter.error_ppformat ~display_format:Human_readable) e in
       failwith ("Error compiling the testlib: " ^ error_msg)
  else
    match Simple_utils.Trace.to_stdlib_result @@
            Ligo_compile.Utils.type_contract_string ~add_warning:(fun _ -> ()) ~options CameLIGO (lib_stub syntax) with
    | Ok s -> s
    | Error e ->
       let error_msg = Format.asprintf "%a" (Main_errors.Formatter.error_ppformat ~display_format:Human_readable) e in
       failwith ("Error compiling the testlib: " ^ error_msg)

module LanguageMap = Simple_utils.Map.Make(struct type t = Syntax_types.t * bool let compare (t, b) (t', b') = match Syntax_types.compare t t' with 0 -> compare_bool b b' | c -> c end)
let cached = ref LanguageMap.empty

let typed ~options syntax =
  Helpers.internalize_typed @@
    match LanguageMap.find_opt (syntax, options.Compiler_options.middle_end.test) @@ ! cached with
    | None ->
       let typed, core = testlib ~options syntax in
       cached := LanguageMap.add (syntax, options.Compiler_options.middle_end.test) (typed, core) @@ ! cached;
       typed
    | Some (typed, _) -> typed

let core ~options syntax =
  Helpers.internalize_core @@
    match LanguageMap.find_opt (syntax, options.Compiler_options.middle_end.test) @@ ! cached with
    | None ->
       let typed, core = testlib ~options syntax in
       cached := LanguageMap.add (syntax, options.Compiler_options.middle_end.test) (typed, core) @@ ! cached;
       core
    | Some (_, core) -> core
