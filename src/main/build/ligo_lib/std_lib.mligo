let failwith (type a b) = [%Michelson ({|{ FAILWITH }|} : a -> b)]

type bool = True | False
type 'a option = Some of 'a | None

module Tezos = struct

  let get_balance (_u : unit) : tez = [%Michelson ({| { DROP ; BALANCE } |} : unit -> tez)] ()
  let get_amount (_u : unit) : tez = [%Michelson ({| { DROP ; AMOUNT } |} : unit -> tez)] ()
  let get_now (_u : unit) : timestamp = [%Michelson ({| { DROP ; NOW } |} : unit -> timestamp)] ()
  let get_sender (_u : unit) : address = [%Michelson ({| { DROP ; SENDER } |} : unit -> address)] ()
  let get_source (_u : unit) : address = [%Michelson ({| { DROP ; SOURCE } |} : unit -> address)] ()
  let get_level (_u : unit) : nat = [%Michelson ({| { DROP ; LEVEL } |} : unit -> nat)] ()
  let get_self_address (_u : unit) : address = [%Michelson ({| { DROP ; SELF_ADDRESS } |} : unit -> address)] ()
  let get_chain_id (_u : unit) : chain_id = [%Michelson ({| { DROP ; CHAIN_ID } |} : unit -> chain_id)] ()
  let get_total_voting_power (_u : unit) : nat = [%Michelson ({| { DROP ; TOTAL_VOTING_POWER } |} : unit -> nat)] ()
  let get_min_block_time (_u : unit) : nat = [%Michelson ({| { DROP; MIN_BLOCK_TIME } |} : unit -> nat) ] ()
  let voting_power (kh : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat)] kh
  let address (type a) (c : a contract) : address = [%Michelson ({| { ADDRESS } |} : a contract -> address)] c
  let implicit_account (kh : key_hash) : unit contract = [%Michelson ({| { IMPLICIT_ACCOUNT } |} : key_hash -> unit contract)] kh
  let join_tickets (type a) (t : a ticket * a ticket) : (a ticket) option = [%Michelson ({| { JOIN_TICKETS } |} : a ticket * a ticket -> a ticket option)] t
  let read_ticket (type a) (t : a ticket) : (address * (a * nat)) * a ticket =
    [%Michelson ({| { READ_TICKET ; PAIR } |} : a ticket -> (address * (a * nat)) * a ticket)] t
  let never (type a) (n : never) : a = [%Michelson ({| { NEVER } |} : never -> a)] n
  let pairing_check (l : (bls12_381_g1 * bls12_381_g2) list) : bool = [%Michelson ({| { PAIRING_CHECK } |} : (bls12_381_g1 * bls12_381_g2) list -> bool)] l
  let set_delegate (o : key_hash option) : operation = [%Michelson ({| { SET_DELEGATE } |} : key_hash option -> operation)] o
  [@inline] [@thunk] let self (type a) (s : string) : a contract =
    let _ : a option = [%external ("CHECK_SELF", s)] in
    [%Michelson (({| { DROP ; SELF (annot $0) } |} : unit -> a contract), (s : string))] ()
  [@inline] [@thunk] let constant (type a) (s : string) : a = [%external ("GLOBAL_CONSTANT", s)]
  [@inline] [@thunk] let sapling_empty_state (type sap_a) : sap_a sapling_state =
    [%Michelson (({| { DROP ; SAPLING_EMPTY_STATE (type $0) } |} : unit -> sap_a sapling_state), (() : sap_a))] ()
  [@inline] [@thunk] let get_contract_opt (type p) (a : address) : (p contract) option =
    [%Michelson (({| { CONTRACT (type $0) } |} : address -> (p contract) option), (() : p))] a
  [@inline] [@thunk] let get_contract (type a) (a : address) : (a contract) =
    let v = get_contract_opt a in
    match v with | None -> failwith "bad address for get_contract" | Some c -> c

  let get_contract_with_error (type a) (a : address) (s : string) : a contract =
    let v = get_contract_opt a in
    match v with | None -> failwith s | Some c -> c
  let create_ticket (type a) (v : a) (n : nat) : (a ticket) option = [%Michelson ({| { UNPAIR ; TICKET } |} : a * nat -> (a ticket) option)] (v, n)
  let transaction (type a) (a : a) (mu : tez) (c : a contract) : operation =
    [%Michelson ({| { UNPAIR ; UNPAIR ; TRANSFER_TOKENS } |} : a * tez * a contract -> operation)] (a, mu, c)
  [@inline] [@thunk] let call_view (type a b) (s : string) (x : a) (a : address)  : b option =
    [%Michelson (({| { UNPAIR ; VIEW (litstr $0) (type $1) } |} : a * address -> b option), (s : string), (() : b))] (x, a)
  let split_ticket (type a) (t : a ticket) (p : nat * nat) : (a ticket * a ticket) option =
    [%Michelson ({| { UNPAIR ; SPLIT_TICKET } |} : a ticket * (nat * nat) -> (a ticket * a ticket) option)] (t, p)
  [@inline] [@thunk] let create_contract (type p s) (f : p -> s -> operation list * s) (kh : key_hash option) (t : tez) (s : s) : (operation * address) =
      let uncurry (type a b c) (f : a -> b -> c) (xy : a * b) : c = f xy.0 xy.1 in
      [%external ("CREATE_CONTRACT", uncurry f, kh, t, s)]
  [@inline] [@thunk] let create_contract_uncurried (type p s) (f : p * s -> operation list * s) (kh : key_hash option) (t : tez) (s : s) : (operation * address) =
      [%external ("CREATE_CONTRACT", f, kh, t, s)]
  [@inline] [@thunk] let get_entrypoint_opt (type p) (e : string) (a : address) : p contract option =
    let _ : unit = [%external ("CHECK_ENTRYPOINT", e)] in
    [%Michelson (({| { CONTRACT (annot $0) (type $1) } |} : address -> (p contract) option), (e : string), (() : p))] a
  [@inline] [@thunk] let get_entrypoint (type p) (e : string) (a : address) : p contract =
    let v = get_entrypoint_opt e a in
    match v with | None -> failwith "bad address for get_entrypoint" | Some c -> c
  [@inline] [@thunk] let emit (type a) (s : string) (v : a) : operation =
    let _ : unit = [%external ("CHECK_EMIT_EVENT", s, v)] in
    [%Michelson (({| { EMIT (annot $0) (type $1) } |} : a -> operation), (s : string), (() : a))] v
  [@inline] [@thunk] let sapling_verify_update (type sap_a) (t : sap_a sapling_transaction) (s : sap_a sapling_state) : (bytes * (int * sap_a sapling_state)) option = [%Michelson ({| { UNPAIR ; SAPLING_VERIFY_UPDATE } |} : (sap_a sapling_transaction) * (sap_a sapling_state) -> (bytes * (int * sap_a sapling_state)) option)] (t, s)

end

module Bitwise = struct
  let @and        (type a b) (l : a) (r : b) : (a, b) external_and = [%Michelson ({| { UNPAIR ; AND } |} : a * b -> (a, b) external_and)] (l, r)
  let xor         (type a b) (l : a) (r : b) : (a, b) external_or  = [%Michelson ({| { UNPAIR ; XOR } |} : a * b -> (a, b) external_or )] (l, r)
  let @or         (type a b) (l : a) (r : b) : (a, b) external_xor = [%Michelson ({| { UNPAIR ; OR  } |} : a * b -> (a, b) external_xor)] (l, r)
  let shift_left  (type a b) (l : a) (r : b) : (a, b) external_lsl = [%Michelson ({| { UNPAIR ; LSL } |} : a * b -> (a, b) external_lsl)] (l, r)
  let shift_right (type a b) (l : a) (r : b) : (a, b) external_lsr = [%Michelson ({| { UNPAIR ; LSR } |} : a * b -> (a, b) external_lsr)] (l, r)
end

module Big_map = struct
  [@inline] let empty (type k v) : (k, v) big_map = [%external ("BIG_MAP_EMPTY")]
  [@thunk] [@inline] let literal (type k v) (l : (k * v) list) : (k, v) big_map = [%external ("BIG_MAP_LITERAL", l)]

  let mem (type k v) (k : k) (m : (k, v) big_map) : bool = [%external ("MAP_MEM", k, m)]
  let add (type k v) (k : k) (v : v) (m : (k, v) big_map) : (k, v) big_map = [%external ("MAP_ADD", k, v, m)]
  let remove (type k v) (k : k) (m : (k, v) big_map) : (k, v) big_map = [%external ("MAP_REMOVE", k, m)]
  let update (type k v) (k : k) (v : v option) (m : (k, v) big_map) : (k, v) big_map = [%external ("MAP_UPDATE", k, v, m)]
  let get_and_update (type k v) (k : k) (v : v option) (m : (k, v) big_map) : v option * (k, v) big_map = [%external ("BIG_MAP_GET_AND_UPDATE", k, v, m)]
  let find_opt (type k v) (k : k) (m : (k, v) big_map) : v option = [%external ("MAP_FIND_OPT", k, m)]
  let find (type k v) (k : k) (m : (k, v) big_map) : v = [%external ("MAP_FIND", k, m)]

end

module Map = struct
  let empty (type k v) : (k, v) map = [%external ("MAP_EMPTY")]
  let size (type k v) (m : (k, v) map) : nat = [%external ("MAP_SIZE", m)]
  [@thunk] [@inline] let literal (type k v) (l : (k * v) list) : (k, v) map = [%external ("MAP_LITERAL", l)]

  let mem (type k v) (k : k) (m : (k, v) map) : bool = [%external ("MAP_MEM", k, m)]
  let add (type k v) (k : k) (v : v) (m : (k, v) map) : (k, v) map = [%external ("MAP_ADD", k, v, m)]
  let remove (type k v) (k : k) (m : (k, v) map) : (k, v) map = [%external ("MAP_REMOVE", k, m)]
  let update (type k v) (k : k) (v : v option) (m : (k, v) map) : (k, v) map = [%external ("MAP_UPDATE", k, v, m)]
  let get_and_update (type k v) (k : k) (v : v option) (m : (k, v) map) : v option * (k, v) map = [%external ("MAP_GET_AND_UPDATE", k, v, m)]
  let find (type k v) (k : k) (m : (k, v) map) : v = [%external ("MAP_FIND", k, m)]
  let find_opt (type k v) (k : k) (m : (k, v) map) : v option = [%external ("MAP_FIND_OPT", k, m)]
  let iter (type k v) (f : k * v -> unit) (m : (k, v) map) : unit = [%external ("MAP_ITER", f, m)]
  let map (type k v w) (f : k * v -> w) (m : (k, v) map) : (k, w) map = [%external ("MAP_MAP", f, m)]
  let fold (type k v c) (f : c * (k * v) -> c) (m : (k, v) map) (i : c) : c = [%external ("MAP_FOLD", f, m, i)]

end

module Transpiled = struct
  let map_find_opt (type k b) (k : k) (m : b) : (k, b) external_map_find_opt = [%Michelson ({| { UNPAIR ; GET } |} : k * b -> (k, b) external_map_find_opt)] (k, m)
  let map_add (type k v b) (k : k) (v : v) (m : b) : (k, v, b) external_map_add = [%Michelson ({| { UNPAIR ; UNPAIR ; DIP { SOME } ; UPDATE } |} : k * v * b -> (k, v, b) external_map_add)] (k, v, m)
  let map_remove (type k b) (k : k) (m : b) : (k, b) external_map_remove = [%Michelson (({| { UNPAIR ; DIP { NONE (type $0) } ; UPDATE } |} : k * b -> (k, b) external_map_remove), (() : (k, b) external_map_remove_value))] (k, m)
end

module Set = struct
  let empty (type a) : a set = [%external ("SET_EMPTY")]
  let size (type a) (s : a set) : nat = [%external ("SET_SIZE", s)]
  let cardinal (type a) (s : a set) : nat = [%external ("SET_SIZE", s)]
  [@thunk] [@inline] let literal (type a) (l : a list) : a set = [%external ("SET_LITERAL", l)]

  let mem (type a) (x : a) (s : a set) : bool = [%external ("SET_MEM", x, s)]
  let add (type a) (x : a) (s : a set) : a set = [%external ("SET_ADD", x, s)]
  let remove (type a) (x : a) (s : a set) : a set = [%external ("SET_REMOVE", x, s)]
  let update (type a) (x : a) (b : bool) (s : a set) = [%external ("SET_UPDATE", x, b, s)]
  let iter (type a) (f : a -> unit) (s : a set) : unit = [%external ("SET_ITER", f, s)]
  let fold (type a b) (f : b * a -> b) (s : a set) (i : b) : b = [%external ("SET_FOLD", f, s, i)]
  let fold_desc (type a b) (f : a * b -> b) (s : a set) (i : b) : b = [%external ("SET_FOLD_DESC", f, s, i)]
  let filter_map (type a b) (f : a -> b option) (xs : a set) : b set =
    fold_desc (fun (a : a * b set) -> match f a.0 with | None -> a.1 | Some b -> add b a.1) xs (empty : b set)
end

module List = struct
  let length (type a) (xs : a list) : nat = [%external ("LIST_SIZE", xs)]
  let size (type a) (xs : a list) : nat = [%external ("LIST_SIZE", xs)]
  let head_opt (type a) (xs : a list) : a option = match xs with | [] -> None | (x :: _) -> Some x
  let tail_opt (type a) (xs : a list) : (a list) option = match xs with | [] -> None | (_ :: xs) -> Some xs

  let map (type a b) (f : a -> b) (xs : a list) : b list = [%external ("LIST_MAP", f, xs)]
  let iter (type a) (f : a -> unit) (xs : a list): unit = [%external ("LIST_ITER", f, xs)]
  let fold (type a b) (f : b * a -> b) (xs : a list) (i : b) : b = [%external ("LIST_FOLD", f, xs, i)]
  let fold_left (type a b) (f : b * a -> b) (i : b) (xs : a list) : b = [%external ("LIST_FOLD_LEFT", f, i, xs)]
  let fold_right (type a b) (f : a * b -> b) (xs : a list) (i : b) : b = [%external ("LIST_FOLD_RIGHT", f, xs, i)]
  let cons (type a) (x : a) (xs : a list) : a list = [%external ("CONS", x, xs)]
  let find_opt (type a) (f : a -> bool) (xs : a list) : a option = 
    fold_right (fun (a : a * a option) -> if f a.0 then Some a.0 else a.1) xs None
  let filter_map (type a b) (f : a -> b option) (xs : a list) : b list =
    fold_right (fun (a : a * b list) -> match f a.0 with | None -> a.1 | Some b -> (b :: a.1)) xs []
  let update (type a) (f : a -> a option) (xs : a list) : a list =
    map (fun a -> match f a with | None -> a | Some a -> a) xs
  let update_with (type a) (f : a -> bool) (v : a) (xs : a list) : a list =
    map (fun (a : a) -> if f a then v else a) xs
end

module String = struct
  let length (b : string) : nat = [%external ("SIZE", b)]
  let concats (bs : string list) : string = [%external ("CONCATS", bs)]

  let concat (b1 : string) (b2 : string) : string = [%external ("CONCAT", b1, b2)]
  let sub (s : nat) (l : nat) (b : string) : string = [%external ("SLICE", s, l, b)]
end

module Option = struct
  let unopt (type a) (v : a option) : a = match v with | Some v -> v | None -> failwith "option is None"

  let unopt_with_error (type a) (v : a option) (s : string) : a = match v with | Some v -> v | None -> failwith s
  [@thunk] let map (type a b) (f : a -> b) (v : a option) : b option = [%external ("OPTION_MAP", f, v)]
  let value (type a) (default : a) (v : a option) : a = match v with | None -> default | Some v -> v
  let value_exn (type err a) (err : err) (x : a option) : a = match x with None -> failwith err | Some v -> v
  let is_none (type a) (v : a option) : bool = match v with | None -> True | Some _ -> False
  let is_some (type a) (v : a option) : bool = match v with | None -> False | Some _ -> True
end

module Bytes = struct
  let concats (bs : bytes list) : bytes = [%external ("CONCATS", bs)]
  let pack (type a) (v : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] v
  let unpack (type a) (b : bytes) : a option = [%Michelson (({| { UNPACK (type $0) } |} : bytes -> a option), (() : a))] b
  let length (b : bytes) : nat = [%external ("SIZE", b)]

  let concat (b1 : bytes) (b2 : bytes) : bytes = [%external ("CONCAT", b1, b2)]
  let sub (s : nat) (l : nat) (b : bytes) : bytes = [%external ("SLICE", s, l, b)]
end

module Crypto = struct
  let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
  let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
  let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
  let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
  let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
  let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k

  let check (k : key) (s : signature) (b : bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end

let assert (b : bool) : unit = if b then () else failwith "failed assertion"
let assert_some (type a) (v : a option) : unit = match v with | None -> failwith "failed assert some" | Some _ -> ()
let assert_none (type a) (v : a option) : unit = match v with | None -> () | Some _ -> failwith "failed assert none"
let abs (i : int) : nat = [%Michelson ({| { ABS } |} : int -> nat)] i
let is_nat (i : int) : nat option = [%Michelson ({| { ISNAT } |} : int -> nat option)] i
let true : bool = True
let false : bool = False
let unit : unit = [%external ("UNIT")]
let int (type a) (v : a) : a external_int = [%Michelson ({| { INT } |} : a -> a external_int)] v
let ignore (type a) (_ : a) : unit = ()
let curry (type a b c) (f : a * b -> c) (x : a) (y : b) : c = f (x, y)
let uncurry (type a b c) (f : a -> b -> c) (xy : a * b) : c = f xy.0 xy.1

let assert_with_error (b : bool) (s : string) = if b then () else failwith s
let assert_some_with_error (type a) (v : a option) (s : string) : unit = match v with | None -> failwith s | Some _ -> ()
let assert_none_with_error (type a) (v : a option) (s : string) : unit = match v with | None -> () | Some _ -> failwith s
let ediv (type a b) (l : a) (r : b) : (a, b) external_ediv = [%Michelson ({| { UNPAIR ; EDIV } |} : a * b -> (a, b) external_ediv)] (l, r)


type test_exec_error_balance_too_low =
  { contract_too_low : address ; contract_balance : tez ; spend_request : tez }

type test_exec_error =
  | Rejected of michelson_program * address
  | Balance_too_low of test_exec_error_balance_too_low
  | Other of string

type test_exec_result = Success of nat | Fail of test_exec_error

type test_baker_policy =
  | By_round of int
  | By_account of address
  | Excluding of address list

type 'a pbt_test = ('a pbt_gen) * ('a -> bool)
type 'a pbt_result = Success | Fail of 'a

type 's unforged_ticket = [@layout:comb] { ticketer : address ; value : 's ; amount : nat }

module Test = struct

  let run (type a b) (f : a -> b) (v : a) : michelson_program = [%external ("TEST_RUN", f, v)]
  let eval (type a) (x : a) : michelson_program = run (fun (x : a) -> x) x

  let compile_value (type a) (x : a) : michelson_program = eval x
  let get_total_voting_power (_u : unit) : nat = [%external ("TEST_GET_TOTAL_VOTING_POWER", ())]
  let failwith (type a b) (v : a) : b = [%external ("TEST_FAILWITH", v)]
  let to_contract (type p s) (t : (p, s) typed_address) : p contract = [%external ("TEST_TO_CONTRACT", t)]
  let set_source (a : address) : unit = [%external ("TEST_SET_SOURCE", a)]
  let get_storage_of_address (a : address) : michelson_program = [%external ("TEST_GET_STORAGE_OF_ADDRESS", a)]
  let get_balance (a : address) : tez = [%external ("TEST_GET_BALANCE", a)]
  let print (v : string) : unit = [%external ("TEST_PRINT", 1, v)]
  let eprint (v : string) : unit = [%external ("TEST_PRINT", 2, v)]
  let get_voting_power (kh : key_hash) : nat = [%external ("TEST_GET_VOTING_POWER", kh)]
  let nth_bootstrap_contract (i : nat) : address = [%external ("TEST_NTH_BOOTSTRAP_CONTRACT", i)]
  let nth_bootstrap_account (i : int) : address =
    let (a, _, _) = [%external ("TEST_GET_NTH_BS", i)] in
    a
  let get_bootstrap_account (n : nat) : address * key * string = [%external ("TEST_GET_NTH_BS", (int n))]
  let nth_bootstrap_typed_address (type a b) (n : nat) : (a, b) typed_address = [%external ("TEST_NTH_BOOTSTRAP_TYPED_ADDRESS", n)]
  let last_originations (u : unit) : (address, address list) map = [%external ("TEST_LAST_ORIGINATIONS", u)]
  let random (type a) (_u : unit) : a =
    let g : a pbt_gen = [%external ("TEST_RANDOM", false)] in
    [%external ("TEST_GENERATOR_EVAL", g)]
  let new_account (u : unit) : string * key = [%external ("TEST_NEW_ACCOUNT", u)]
  let decompile (type a) (m : michelson_program) : a = [%external ("TEST_DECOMPILE", m)]
  let bake_until_n_cycle_end (n : nat) : unit = [%external ("TEST_BAKE_UNTIL_N_CYCLE_END", n)]
  let get_time (_u : unit) : timestamp = Tezos.get_now ()
  let cast_address (type a b) (a : address) : (a, b) typed_address = [%external ("TEST_CAST_ADDRESS", a)]
  let register_delegate (kh : key_hash) : unit = [%external ("TEST_REGISTER_DELEGATE", kh)]
  let register_constant (m : michelson_program) : string = [%external ("TEST_REGISTER_CONSTANT", m)]
  let to_typed_address (type a b) (c : a contract) : (a, b) typed_address = [%external ("TEST_TO_TYPED_ADDRESS", c)]
  let constant_to_michelson_program (s : string) : michelson_program = [%external ("TEST_CONSTANT_TO_MICHELSON", s)]
  let parse_michelson (s : string) : michelson_program = [%external ("TEST_CONSTANT_TO_MICHELSON", s)]
  let restore_context (u : unit) : unit = [%external ("TEST_POP_CONTEXT", u)]
  let save_context (u : unit) : unit = [%external ("TEST_PUSH_CONTEXT", u)]
  let drop_context (u : unit) : unit = [%external ("TEST_DROP_CONTEXT", u)]
  let to_string (type a) (v : a) : string = [%external ("TEST_TO_STRING", v, 0)]
  let to_json (type a) (v : a) : string = [%external ("TEST_TO_STRING", v, 1)]
  let get_storage (type p s) (t : (p, s) typed_address) : s =
    let c : p contract = to_contract t in
    let a : address = [%external ("TEST_ADDRESS", c)] in
    let s : michelson_program = get_storage_of_address a in
    (decompile s : s)
  let set_baker_policy (bp : test_baker_policy) : unit = [%external ("TEST_SET_BAKER", bp)]
  let set_baker (a : address) : unit = set_baker_policy (By_account a)
  let size (c : michelson_contract) : int = [%external ("TEST_SIZE", c)]
  let compile_contract (type p s) (f : p * s -> operation list * s) : michelson_contract =
    let no_vs : s views = [%external ("TEST_NIL_VIEWS", ())] in
    let ast_c : ast_contract = [%external ("TEST_COMPILE_CONTRACT", f, no_vs)] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]
  let read_contract_from_file (fn : string) : michelson_contract = [%external ("TEST_READ_CONTRACT_FROM_FILE", fn)]
  let chr (n : nat) : string option =
    let backslash = "\\" in
    if n < 10n then
      Some ([%external ("TEST_UNESCAPE_STRING", (backslash ^ "00" ^ to_string (int n)))])
    else if n < 100n then
      Some ([%external ("TEST_UNESCAPE_STRING", (backslash ^ "0" ^ to_string (int n)))])
    else if n < 256n then
      Some ([%external ("TEST_UNESCAPE_STRING", (backslash ^ to_string (int n)))])
    else
      None
  let nl = [%external ("TEST_UNESCAPE_STRING", "\n")]
  let println (v : string) : unit =
    print (v ^ nl)
(* one day we might be able to write  `[@private] let print_values : ref bool = true` or something *)
  let set_print_values (_ : unit) : unit = let _ = [%external ("TEST_SET_PRINT_VALUES", true)] in ()
  let unset_print_values (_ : unit) : unit = let _ = [%external ("TEST_SET_PRINT_VALUES", false)] in ()

  module PBT = struct
    let gen (type a) : a pbt_gen = [%external ("TEST_RANDOM", false)]
    let gen_small (type a) : a pbt_gen = [%external ("TEST_RANDOM", true)]
    let make_test (type a) (g : a pbt_gen) (p : a -> bool) : a pbt_test = (g, p)
    let run (type a) ((g, p) : a pbt_test) (k : nat) : a pbt_result =
      let iter = fun ((n, _) : nat * a pbt_result) ->
                                       if n = k then
                                         [%external ("LOOP_STOP", (0n, (Success : a pbt_result)))]
                                       else
                                         let v = [%external ("TEST_GENERATOR_EVAL", g)] in
                                         if p v then
                                           [%external ("LOOP_CONTINUE", ((n + 1n), (Success : a pbt_result)))]
                                         else
                                           [%external ("LOOP_STOP", (n, Fail v))] in
      let (_, v) = [%external ("LOOP_LEFT", iter, (0n, (Success : a pbt_result)))] in
      v
  end

  let get_last_events_from (type a p s) (addr : (p,s) typed_address) (rtag: string) : a list =
    let addr = Tezos.address (to_contract addr) in
    let event_map : (address * a) list = [%external ("TEST_LAST_EVENTS", rtag)] in
    let f ((acc, (c_addr,event)) : a list * (address * a)) : a list =
      if addr = c_addr then event::acc
      else acc
    in
    List.fold f event_map ([]: a list)
  let transfer (a : address) (s : michelson_program) (t : tez) : test_exec_result = [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", a, (None : string option), s, t)]
  let transfer_exn (a : address) (s : michelson_program) (t : tez) : nat = [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS_EXN", a, (None : string option), s, t)]
  let log (type a) (v : a) : unit =
    let nl = [%external ("TEST_UNESCAPE_STRING", "\n")] in
    let s = to_string v ^ nl in
    print s
  let reset_state (n : nat) (l : tez list) : unit = [%external ("TEST_STATE_RESET", (None : timestamp option), n, l)]
  let reset_state_at (t:timestamp) (n : nat) (l : tez list) : unit = [%external ("TEST_STATE_RESET", (Some t), n, l)]
  let bootstrap_contract (type p s) (f : p * s -> operation list * s) (s : s) (t : tez) : unit = [%external ("TEST_BOOTSTRAP_CONTRACT", f, s, t)]
  let mutate_value (type a) (n : nat) (v : a) : (a * mutation) option = [%external ("TEST_MUTATE_VALUE", n, v)]
  let save_mutation (s : string) (m : mutation) : string option = [%external ("TEST_SAVE_MUTATION", s, m)]
  let sign (sk : string) (d : bytes) : signature = [%external ("TEST_SIGN", sk, d)]
  let add_account (s : string) (k : key) : unit = [%external ("TEST_ADD_ACCOUNT", s, k)]
  let baker_account (p : string * key) (o : tez option) : unit = [%external ("TEST_BAKER_ACCOUNT", p, o)]
  let set_big_map (type a b) (i : int) (m : (a, b) big_map) : unit = [%external ("TEST_SET_BIG_MAP", i, m)]
  let transfer_to_contract (type p) (c : p contract) (s : p) (t : tez) : test_exec_result =
    let a : address = [%external ("TEST_ADDRESS", c)] in
    let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
    let s : michelson_program = eval s in
    [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", a, e, s, t)]
  let transfer_to_contract_exn (type p) (c : p contract) (s : p) (t : tez) : nat =
      let a : address = [%external ("TEST_ADDRESS", c)] in
      let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
      let s : michelson_program = eval s in
      [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS_EXN", a, e, s, t)]
  let michelson_equal (m1 : michelson_program) (m2 : michelson_program) : bool = m1 = m2
  let to_entrypoint (type a b c) (s : string) (t : (a, b) typed_address) : c contract =
    let s = if String.length s > 0n then
              if String.sub 0n 1n s = "%" then
                let () = log "WARNING: Test.to_entrypoint: automatically removing starting %" in
                String.sub 1n (abs (String.length s - 1)) s
	      else s
	    else s in
    [%external ("TEST_TO_ENTRYPOINT", s, t)]
  let originate_contract (c : michelson_contract) (s : michelson_program) (t : tez) : address = [%external ("TEST_ORIGINATE", c, s, t)]
  let originate (type p s) (f : p -> s -> operation list * s) (s : s) (t : tez) : ((p, s) typed_address * michelson_contract * int) =
    let f = compile_contract (uncurry f) in
    let s = eval s in
    let a = originate_contract f s t in
    let c = size f in
    let a : (p, s) typed_address = cast_address a in
    (a, f, c)
    let compile_contract_with_views (type p s) (f : p * s -> operation list * s) (vs : s views) : michelson_contract =
      let ast_c : ast_contract = [%external ("TEST_COMPILE_CONTRACT", f, vs)] in
      [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]
  let originate_uncurried (type p s) (f : p * s -> operation list * s) (s : s) (t : tez) : ((p, s) typed_address * michelson_contract * int) =
    let f = compile_contract f in
    let s = eval s in
    let a = originate_contract f s t in
    let c = size f in
    let a : (p, s) typed_address = cast_address a in
    (a, f, c)
  let originate_module (type p s) ((f, vs) : (p * s -> operation list * s) * s views) (s : s) (t : tez) : ((p, s) typed_address * michelson_contract * int) =
    let f = compile_contract_with_views f vs in
    let s = eval s in
    let a = originate_contract f s t in
    let c = size f in
    let a : (p, s) typed_address = cast_address a in
    (a, f, c)
  let compile_contract_from_file (fn : string) (e : string) (v : string list) : michelson_contract =
    let ast_c : ast_contract = [%external ("TEST_COMPILE_CONTRACT_FROM_FILE", fn, e, v, (None : nat option))] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]
  let originate_from_file (fn : string) (e : string) (v : string list) (s : michelson_program)  (t : tez) : address * michelson_contract * int =
    let f = compile_contract_from_file fn e v in
    let a = originate_contract f s t in
    let c = size f in
    (a, f, c)
  let mutation_test (type a b) (v : a) (tester : a -> b) : (b * mutation) option =
    let try_with (type a) (v : unit -> a) (c : unit -> a) = [%external ("TEST_TRY_WITH", v, c)] in
    type ret_code = Passed of (b * mutation) | Continue | Stop in
    let rec mutation_nth (n : nat) : (b * mutation) option =
      let curr = match mutate_value n v with
        | Some (v, m) -> try_with (fun (_ : unit) -> let b = tester v in Passed (b, m)) (fun (_ : unit) -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> None
      | Continue -> mutation_nth (n + 1n)
      | Passed (b, m) -> Some (b, m) in
    mutation_nth 0n
  let mutation_test_all (type a b) (v : a) (tester : a -> b) : (b * mutation) list =
    let try_with (type a) (v : unit -> a) (c : unit -> a) = [%external ("TEST_TRY_WITH", v, c)] in
    type ret_code = Passed of (b * mutation) | Continue | Stop in
    let rec mutation_nth (acc : (b * mutation) list) (n : nat) : (b * mutation) list =
      let curr = match mutate_value n v with
        | Some (v, m) -> try_with (fun (_ : unit) -> let b = tester v in Passed (b, m)) (fun (_ : unit) -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> acc
      | Continue -> mutation_nth acc (n + 1n)
      | Passed (b, m) -> mutation_nth ((b, m) :: acc) (n + 1n) in
    mutation_nth ([] : (b * mutation) list) 0n
  let originate_from_file_and_mutate (type b) (fn : string) (e : string) (v : string list) (s : michelson_program) (t : tez)
                                     (tester : address * michelson_contract * int -> b) : (b * mutation) option =
    let wrap_tester (v : ast_contract) : b =
      let f = [%external ("TEST_COMPILE_AST_CONTRACT", v)] in
      let a = originate_contract f s t in
      let c = size f in
      tester (a, f, c) in
    let ast_c : ast_contract = [%external ("TEST_COMPILE_CONTRACT_FROM_FILE", fn, e, v, (None : nat option))] in
    let try_with (type a) (v : unit -> a) (c : unit -> a) = [%external ("TEST_TRY_WITH", v, c)] in
    type ret_code = Passed of (b * mutation) | Continue | Stop in
    let rec mutation_nth (n : nat) : (b * mutation) option =
      let mutated = [%external ("TEST_MUTATE_CONTRACT", n, ast_c)] in
      let curr = match mutated with
        | Some (v, m) -> try_with (fun (_ : unit) -> let b = wrap_tester v in Passed (b, m)) (fun (_ : unit) -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> None
      | Continue -> mutation_nth (n + 1n)
      | Passed (b, m) -> Some (b, m) in
    mutation_nth 0n
  let originate_from_file_and_mutate_all (type b) (fn : string) (e : string) (v : string list) (s : michelson_program) (t : tez)
                                         (tester : address * michelson_contract * int -> b) : (b * mutation) list =
    let wrap_tester (v : ast_contract) : b =
      let f = [%external ("TEST_COMPILE_AST_CONTRACT", v)] in
      let a = originate_contract f s t in
      let c = size f in
      tester (a, f, c) in
    let ast_c : ast_contract = [%external ("TEST_COMPILE_CONTRACT_FROM_FILE", fn, e, v, (None : nat option))] in
    let try_with (type a) (v : unit -> a) (c : unit -> a) = [%external ("TEST_TRY_WITH", v, c)] in
    type ret_code = Passed of (b * mutation) | Continue | Stop in
    let rec mutation_nth (acc : (b * mutation) list) (n : nat) : (b * mutation) list =
      let mutated = [%external ("TEST_MUTATE_CONTRACT", n, ast_c)] in
      let curr = match mutated with
        | Some (v, m) -> try_with (fun (_ : unit) -> let b = wrap_tester v in Passed (b, m)) (fun (_ : unit) -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> acc
      | Continue -> mutation_nth acc (n + 1n)
      | Passed (b, m) -> mutation_nth ((b, m) :: acc) (n + 1n) in
    mutation_nth ([] : (b * mutation) list) 0n

  let assert (b : bool) : unit = if b then () else failwith "failed assertion"
  let assert_some (type a) (v : a option) : unit = match v with | None -> failwith "failed assert some" | Some _ -> ()
  let assert_none (type a) (v : a option) : unit = match v with | None -> () | Some _ -> failwith "failed assert none"

  let assert_with_error (b : bool) (s : string) = if b then () else failwith s
  let assert_some_with_error (type a) (v : a option) (s : string) : unit = match v with | None -> failwith s | Some _ -> ()
  let assert_none_with_error (type a) (v : a option) (s : string) : unit = match v with | None -> () | Some _ -> failwith s

end
