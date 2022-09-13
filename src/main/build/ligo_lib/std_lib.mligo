
module Tezos = struct

  let get_balance (_u : unit) : tez = [%Michelson ({| { DROP ; BALANCE } |} : unit -> tez)] ()
  let get_amount (_u : unit) : tez = [%Michelson ({| { DROP ; AMOUNT } |} : unit -> tez)] ()
  let get_now (_u : unit) : timestamp = [%Michelson ({| { DROP ; NOW } |} : unit -> timestamp)] ()
  let get_sender (_u : unit) : address = [%Michelson ({| { DROP ; SENDER } |} : unit -> address)] ()
  let get_source (_u : unit) : address = [%Michelson ({| { DROP ; SOURCE } |} : unit -> address)] ()
  let get_level (_u : unit) : nat = [%Michelson ({| { DROP ; LEVEL } |} : unit -> nat)] ()
  let get_self_address (_u : unit) : address = [%external ("SELF_ADDRESS")]
  let get_chain_id (_u : unit) : chain_id = [%Michelson ({| { DROP ; CHAIN_ID } |} : unit -> chain_id)] ()
  let get_total_voting_power (_u : unit) : nat = [%Michelson ({| { DROP ; TOTAL_VOTING_POWER } |} : unit -> nat)] ()
  let get_min_block_time (_u : unit) : nat = [%Michelson ({| { DROP; MIN_BLOCK_TIME } |} : unit -> nat) ] ()
  let voting_power (kh : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat)] kh
  let address (type a) (c : a contract) : address = [%external ("ADDRESS", c)]
  let implicit_account (kh : key_hash) : unit contract = [%external ("IMPLICIT_ACCOUNT", kh)]
  let join_tickets (type a) (t : a ticket * a ticket) : (a ticket) option = [%Michelson ({| { JOIN_TICKETS } |} : a ticket * a ticket -> a ticket option)] t
  let read_ticket (type a) (t : a ticket) : (address * (a * nat)) * a ticket =
    [%Michelson ({| { READ_TICKET ; PAIR } |} : a ticket -> (address * (a * nat)) * a ticket)] t
  (* let create_contract (type a b) (c : a * b -> operation list * b) (kh : key_hash) (mu : tez) (s : b) : operation * address = [%external ("CREATE_CONTRACT", c, kh, mu, s)] *)
  let never (type a) (n : never) : a = [%Michelson ({| { NEVER } |} : never -> a)] n
  let pairing_check (l : (bls12_381_g1 * bls12_381_g2) list) : bool = [%Michelson ({| { PAIRING_CHECK } |} : (bls12_381_g1 * bls12_381_g2) list -> bool)] l
  let constant (type a) (s : string) : a = [%external ("GLOBAL_CONSTANT", s)]
  let set_delegate (o : key_hash option) : operation = [%external ("SET_DELEGATE", o)]
  [@inline] [@thunk] let self (type a) (s : string) : a contract = [%external ("SELF", s)]
  [@inline] [@thunk] let constant (type a) (s : string) : a = [%external ("GLOBAL_CONSTANT", s)]
  [@inline] [@thunk] let sapling_empty_state (type sap_a) : sap_a sapling_state = [%external ("SAPLING_EMPTY_STATE")]

#if CURRY
  let get_contract (type a) (a : address) : (a contract) = [%external ("CONTRACT", a)]
  let get_contract_opt (type a) (a : address) : (a contract) option = [%external ("CONTRACT_OPT", a)]
  let get_contract_with_error (type a) (a : address) (s : string) : a contract = [%external ("CONTRACT_WITH_ERROR", a, s)]
  let create_ticket (type a) (v : a) (n : nat) : a ticket = [%Michelson ({| { UNPAIR ; TICKET } |} : a * nat -> a ticket)] (v, n)
  let transaction (type a) (a : a) (mu : tez) (c : a contract) : operation = [%external ("CALL", a, mu, c)]
  let open_chest (ck : chest_key) (c : chest) (n : nat) : chest_opening_result = [%external ("OPEN_CHEST", ck, c, n)]
  let call_view (type a b) (s : string) (x : a) (a : address)  : b option = [%external ("VIEW", s, x, a)]
  let split_ticket (type a) (t : a ticket) (p : nat * nat) : (a ticket * a ticket) option =
      [%Michelson ({| { UNPAIR ; SPLIT_TICKET } |} : a ticket * (nat * nat) -> (a ticket * a ticket) option)] (t, p)
  [@inline] [@thunk] let create_contract (type p s) (f : p * s -> operation list * s) (kh : key_hash option) (t : tez) (s : s) : (operation * address) =
      [%external ("CREATE_CONTRACT", f, kh, t, s)]
  [@inline] [@thunk] let get_entrypoint_opt (type p) (e : string) (a : address) : p contract option = [%external ("CONTRACT_ENTRYPOINT_OPT", e, a)]
  [@inline] [@thunk] let get_entrypoint (type p) (e : string) (a : address) : p contract = [%external ("CONTRACT_ENTRYPOINT", e, a)]
  [@inline] [@thunk] let call_view (type a b) (s : string) (v : a) (a : address) : b option = [%external ("VIEW", s, v, a)]
  [@inline] [@thunk] let emit (type a) (s : string) (v : a) : operation = [%external ("EMIT_EVENT", s, v)]
  [@inline] [@thunk] let sapling_verify_update (type sap_a) (t : sap_a sapling_transaction) (s : sap_a sapling_state) : (bytes * (int * sap_a sapling_state)) option = [%external ("SAPLING_VERIFY_UPDATE", t, s)]
#endif

#if UNCURRY
  let get_contract (type a) (a : address) : (a contract) = [%external ("CONTRACT", a)]
  let get_contract_opt (type a) (a : address) : (a contract) option = [%external ("CONTRACT_OPT", a)]
  let get_contract_with_error (type a) ((a, s) : address * string) : a contract = [%external ("CONTRACT_WITH_ERROR", a, s)]
  let create_ticket (type a) ((v, n) : a * nat) : a ticket = [%Michelson ({| { UNPAIR ; TICKET } |} : a * nat -> a ticket)] (v, n)
  let transaction (type a) ((a, mu, c) : a * tez * a contract) : operation = [%external ("CALL", a, mu, c)]
  let open_chest ((ck, c, n) : chest_key * chest * nat) : chest_opening_result = [%external ("OPEN_CHEST", ck, c, n)]
  let call_view (type a b) ((s, x, a) : string * a * address)  : b option = [%external ("VIEW", s, x, a)]
  let split_ticket (type a) ((t, p) : (a ticket) * (nat * nat)) : (a ticket * a ticket) option =
    [%Michelson ({| { UNPAIR ; SPLIT_TICKET } |} : a ticket * (nat * nat) -> (a ticket * a ticket) option)] (t, p)
  [@inline] [@thunk] let create_contract (type p s) ((f, kh, t, s) : (p * s -> operation list * s) * key_hash option * tez * s) : (operation * address) =
      [%external ("CREATE_CONTRACT", f, kh, t, s)]
  [@inline] [@thunk] let get_entrypoint_opt (type p) ((e, a) : string * address) : p contract option = [%external ("CONTRACT_ENTRYPOINT_OPT", e, a)]
  [@inline] [@thunk] let get_entrypoint (type p) ((e, a) : string * address) : p contract = [%external ("CONTRACT_ENTRYPOINT", e, a)]
  [@inline] [@thunk] let call_view (type a b) ((s, v, a) : string * a * address) : b option = [%external ("VIEW", s, v, a)]
  [@inline] [@thunk] let emit (type a) ((s, v) : string * a) : operation = [%external ("EMIT_EVENT", s, v)]
  [@inline] [@thunk] let sapling_verify_update (type sap_a) ((t, s) : sap_a sapling_transaction * sap_a sapling_state) : (bytes * (int * sap_a sapling_state)) option = [%external ("SAPLING_VERIFY_UPDATE", t, s)]
#endif

end

module Bitwise = struct
#if CURRY
  let @and (type a b) (l : a) (r : b) : (a, b) external_and = [%Michelson ({| { UNPAIR ; AND } |} : a * b -> (a, b) external_and)] (l, r)
  let xor (l : nat) (r : nat) : nat = [%external ("XOR", l, r)]
  let @or (l : nat) (r : nat) : nat = [%external ("OR", l, r)]
  let shift_left (l : nat) (r : nat) : nat = [%external ("LSL", l, r)]
  let shift_right (l : nat) (r : nat) : nat = [%external ("LSR", l, r)]
#endif

#if UNCURRY
  let @and (type a b) ((l, r) : (a * b)) : (a, b) external_u_and = [%Michelson ({| { UNPAIR ; AND } |} : a * b -> (a, b) external_u_and)] (l, r)
  let xor ((l, r) : nat * nat) : nat = [%external ("XOR", l, r)]
  let @or ((l, r) : nat * nat) : nat = [%external ("OR", l, r)]
  let shift_left ((l, r) : nat * nat) : nat = [%external ("LSL", l, r)]
  let shift_right ((l, r) : nat * nat) : nat = [%external ("LSR", l, r)]
#endif
end

module Big_map = struct
  [@inline] let empty (type k v) : (k, v) big_map = [%external ("BIG_MAP_EMPTY")]
  [@thunk] [@inline] let literal (type k v) (l : (k * v) list) : (k, v) big_map = [%external ("BIG_MAP_LITERAL", l)]

#if CURRY
  let mem (type k v) (k : k) (m : (k, v) big_map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) big_map -> bool)] (k, m)
  let add (type k v) (k : k) (v : v) (m : (k, v) big_map) : (k, v) big_map = [%external ("MAP_ADD", k, v, m)]
  let remove (type k v) (k : k) (m : (k, v) big_map) : (k, v) big_map = [%external ("MAP_REMOVE", k, m)]
  let update (type k v) (k : k) (v : v option) (m : (k, v) big_map) : (k, v) big_map = [%external ("MAP_UPDATE", k, v, m)]
  let get_and_update (type k v) (k : k) (v : v option) (m : (k, v) big_map) : v option * (k, v) big_map = [%external ("BIG_MAP_GET_AND_UPDATE", k, v, m)]
  let find_opt (type k v) (k : k) (m : (k, v) big_map) : v option = [%external ("MAP_FIND_OPT", k, m)]
  let find (type k v) (k : k) (m : (k, v) big_map) : v = [%external ("MAP_FIND", k, m)]
#endif

#if UNCURRY
  let mem (type k v) ((k, m) : k * (k, v) big_map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) big_map -> bool)] (k, m)
  let add (type k v) ((k, v, m) : k * v * (k, v) big_map) : (k, v) big_map = [%external ("MAP_ADD", k, v, m)]
  let remove (type k v) ((k, m) : k * (k, v) big_map) : (k, v) big_map = [%external ("MAP_REMOVE", k, m)]
  let update (type k v) ((k, v, m) : k * v option * (k, v) big_map) : (k, v) big_map = [%external ("MAP_UPDATE", k, v, m)]
  let get_and_update (type k v) ((k, v, m) : k * v option * (k, v) big_map) : v option * (k, v) big_map = [%external ("BIG_MAP_GET_AND_UPDATE", k, v, m)]
  let find_opt (type k v) ((k, m) : k * (k, v) big_map) : v option = [%external ("MAP_FIND_OPT", k, m)]
  let find (type k v) ((k, m) : k * (k, v) big_map) : v = [%external ("MAP_FIND", k, m)]
#endif

end

module Map = struct
  let empty (type k v) : (k, v) map = [%external ("MAP_EMPTY")]
  let size (type k v) (m : (k, v) map) : nat = [%Michelson ({| { SIZE } |} : (k, v) map -> nat)] m
  [@thunk] [@inline] let literal (type k v) (l : (k * v) list) : (k, v) map = [%external ("MAP_LITERAL", l)]

#if CURRY
  let mem (type k v) (k : k) (m : (k, v) map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) map -> bool)] (k, m)
  let add (type k v) (k : k) (v : v) (m : (k, v) map) : (k, v) map = [%external ("MAP_ADD", k, v, m)]
  let remove (type k v) (k : k) (m : (k, v) map) : (k, v) map = [%external ("MAP_REMOVE", k, m)]
  let update (type k v) (k : k) (v : v option) (m : (k, v) map) : (k, v) map = [%external ("MAP_UPDATE", k, v, m)]
  let get_and_update (type k v) (k : k) (v : v option) (m : (k, v) map) : v option * (k, v) map = [%external ("MAP_GET_AND_UPDATE", k, v, m)]
  let find (type k v) (k : k) (m : (k, v) map) : v = [%external ("MAP_FIND", k, m)]
  let find_opt (type k v) (k : k) (m : (k, v) map) : v option = [%external ("MAP_FIND_OPT", k, m)]
  let iter (type k v) (f : k * v -> unit) (m : (k, v) map) : unit = [%external ("MAP_ITER", f, m)]
  let map (type k v w) (f : k * v -> w) (m : (k, v) map) : (k, w) map = [%external ("MAP_MAP", f, m)]
  let fold (type k v c) (f : c * (k * v) -> c) (m : (k, v) map) (i : c) : c = [%external ("MAP_FOLD", f, m, i)]
#endif

#if UNCURRY
  let mem (type k v) ((k, m) : k * (k, v) map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) map -> bool)] (k, m)
  let add (type k v) ((k, v, m) : k * v * (k, v) map) : (k, v) map = [%external ("MAP_ADD", k, v, m)]
  let remove (type k v) ((k, m) : k * (k, v) map) : (k, v) map = [%external ("MAP_REMOVE", k, m)]
  let update (type k v) ((k, v, m) : k * v option * (k, v) map) : (k, v) map = [%external ("MAP_UPDATE", k, v, m)]
  let get_and_update (type k v) ((k, v, m) : k * v option * (k, v) map) : v option * (k, v) map = [%external ("MAP_GET_AND_UPDATE", k, v, m)]
  let find_opt (type k v) ((k, m) : k * (k, v) map) : v option = [%external ("MAP_FIND_OPT", k, m)]
  let find (type k v) ((k, m) : k * (k, v) map) : v = [%external ("MAP_FIND", k, m)]
  let iter (type k v) ((f, m) : (k * v -> unit) * (k, v) map) : unit = [%external ("MAP_ITER", f, m)]
  let map (type k v w) ((f, m) : (k * v -> w) * (k, v) map) : (k, w) map = [%external ("MAP_MAP", f, m)]
  let fold (type k v c) ((f, m, i) : (c * (k * v) -> c) * (k, v) map * c) : c = [%external ("MAP_FOLD", f, m, i)]
#endif

end

module Set = struct
  let empty (type a) : a set = [%external ("SET_EMPTY")]
  let size (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)]  s
  let cardinal (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)] s
  [@thunk] [@inline] let literal (type a) (l : a list) : a set = [%external ("SET_LITERAL", l)]

#if CURRY
  let mem (type a) (x : a) (s : a set) : bool = [%external ("SET_MEM", x, s)]
  let add (type a) (x : a) (s : a set) : a set = [%external ("SET_ADD", x, s)]
  let remove (type a) (x : a) (s : a set) : a set = [%external ("SET_REMOVE", x, s)]
  let update (type a) (x : a) (b : bool) (s : a set) = [%external ("SET_UPDATE", x, b, s)]
  let iter (type a) (f : a -> unit) (s : a set) : unit = [%external ("SET_ITER", f, s)]
  let fold (type a b) (f : b * a -> b) (s : a set) (i : b) : b = [%external ("SET_FOLD", f, s, i)]
  let fold_desc (type a b) (f : a * b -> b) (s : a set) (i : b) : b = [%external ("SET_FOLD_DESC", f, s, i)]
#endif

#if UNCURRY
  let mem (type a) ((x, s) : a * a set) : bool = [%external ("SET_MEM", x, s)]
  let add (type a) ((x, s) : a * a set) : a set = [%external ("SET_ADD", x, s)]
  let remove (type a) ((x, s) : a * a set) : a set = [%external ("SET_REMOVE", x, s)]
  let update (type a) ((x, b, s) : a * bool * a set) = [%external ("SET_UPDATE", x, b, s)]
  let iter (type a) ((f, s) : (a -> unit) * a set) : unit = [%external ("SET_ITER", f, s)]
  let fold (type a b) ((f, s, i) : (b * a -> b) * a set * b) : b = [%external ("SET_FOLD", f, s, i)]
  let fold_desc (type a b) ((f, s, i) : (a * b -> b) * a set * b) : b = [%external ("SET_FOLD_DESC", f, s, i)]
#endif

end

module List = struct
  let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  let size (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  let head_opt (type a) (xs : a list) : a option = match xs with | [] -> None | (x :: _) -> Some x
  let tail_opt (type a) (xs : a list) : (a list) option = match xs with | [] -> None | (_ :: xs) -> Some xs

#if CURRY
  let map (type a b) (f : a -> b) (xs : a list) : b list = [%external ("LIST_MAP", f, xs)]
  let iter (type a) (f : a -> unit) (xs : a list): unit = [%external ("LIST_ITER", f, xs)]
  let fold (type a b) (f : b * a -> b) (xs : a list) (i : b) : b = [%external ("LIST_FOLD", f, xs, i)]
  let fold_left (type a b) (f : b * a -> b) (i : b) (xs : a list) : b = [%external ("LIST_FOLD_LEFT", f, i, xs)]
  let fold_right (type a b) (f : a * b -> b) (xs : a list) (i : b) : b = [%external ("LIST_FOLD_RIGHT", f, xs, i)]
  let cons (type a) (x : a) (xs : a list) : a list = [%external ("CONS", x, xs)]
  let find_opt (type a) (f : a -> bool) (xs : a list) : a option = 
    fold_right (fun (a : a * a option) -> if f a.0 then Some a.0 else a.1) xs None
#endif

#if UNCURRY
  let map (type a b) ((f, xs) : (a -> b) * a list) : b list = [%external ("LIST_MAP", f, xs)]
  let iter (type a) ((f, xs) : (a -> unit) * a list): unit = [%external ("LIST_ITER", f, xs)]
  let fold (type a b) ((f, xs, i) : (b * a -> b) * a list * b) : b = [%external ("LIST_FOLD", f, xs, i)]
  let fold_left (type a b) ((f, i, xs) : (b * a -> b) * b * a list) : b = [%external ("LIST_FOLD_LEFT", f, i, xs)]
  let fold_right (type a b) ((f, xs, i) : (a * b -> b) * a list * b) : b = [%external ("LIST_FOLD_RIGHT", f, xs, i)]
  let cons (type a) ((x, xs) : a * a list) : a list = [%external ("CONS", x, xs)]
  let find_opt (type a) ((f, xs) : (a -> bool) * a list) : a option = 
    fold_right ((fun (a : a * a option) -> if f a.0 then Some a.0 else a.1), xs, None)
#endif

end

module String = struct
  let length (b : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] b

#if CURRY
  let concat (b1 : string) (b2 : string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] (b1, b2)
  let sub (s : nat) (l : nat) (b : string) : string = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |} : nat * nat * string -> string)] (s, l, b)
#endif

#if UNCURRY
  let concat ((b1, b2) : string * string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] (b1, b2)
  let sub ((s, l, b) : nat * nat * string) : string = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |} : nat * nat * string -> string)] (s, l, b)
#endif

end

module Option = struct
  let unopt (type a) (v : a option) : a = [%external ("UNOPT", v)]

#if CURRY
  let unopt_with_error (type a) (v : a option) (s : string) : a = [%external ("UNOPT_WITH_ERROR", v, s)]
  [@thunk] let map (type a b) (f : a -> b) (v : a option) : b option = [%external ("OPTION_MAP", f, v)]
#endif

#if UNCURRY
  let unopt_with_error (type a) ((v, s) : (a option) * string) : a = [%external ("UNOPT_WITH_ERROR", v, s)]
  [@thunk] let map (type a b) ((f, v) : (a -> b) * (a option)) : b option = [%external ("OPTION_MAP", f, v)]
#endif

end

module Bytes = struct
  let pack (type a) (v : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] v
  let unpack (type a) (b : bytes) : a option = [%external ("BYTES_UNPACK", b)]
  let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b

#if CURRY
  let concat (b1 : bytes) (b2 : bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] (b1, b2)
  let sub (s : nat) (l : nat) (b : bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (s, l, b)
#endif

#if UNCURRY
  let concat ((b1, b2) : bytes * bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] (b1, b2)
  let sub ((s, l, b) : nat * nat * bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (s, l, b)
#endif

end

module Crypto = struct
  let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
  let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
  let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
  let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
  let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
  let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k

#if CURRY
  let check (k : key) (s : signature) (b : bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
#endif

#if UNCURRY
  let check ((k, s, b) : key * signature * bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
#endif

end

[@private]
  let assert (b : bool) : unit = [%Michelson ({| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |} : bool -> unit)] b
[@private]
  let assert_some (type a) (v : a option) : unit =
    [%Michelson ({| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |} : a option -> unit)] v
[@private]
  let assert_none (type a) (v : a option) : unit =
    [%Michelson ({| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |} : a option -> unit)] v
[@private]
  let abs (i : int) : nat = [%Michelson ({| { ABS } |} : int -> nat)] i
[@private]
  let is_nat (i : int) : nat option = [%Michelson ({| { ISNAT } |} : int -> nat option)] i
[@private]
  let true : bool = [%external ("TRUE")]
[@private]
  let false : bool = [%external ("FALSE")]
[@private]
  let unit : unit = [%external ("UNIT")]
[@private]
  let failwith (type a b) = [%Michelson ({|{ FAILWITH }|} : a -> b)]
[@private]
  let int (type a) (v : a) : a external_int = [%Michelson ({| { INT } |} : a -> a external_int)] v
[@private]
  let ignore (type a) (_ : a) : unit = ()

#if CURRY
[@private]
  let assert_with_error (b : bool) (s : string) =
    [%Michelson ({| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |} : bool * string -> unit)] (b, s)
[@private]
  let assert_some_with_error (type a) (v : a option) (s : string) : unit =
    [%Michelson ({| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |} : a option * string -> unit)] (v, s)
[@private]
  let assert_none_with_error (type a) (v : a option) (s : string) : unit =
    [%Michelson ({| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |} : a option * string -> unit)] (v, s)
[@private]
  let ediv (type a b) (l : a) (r : b) : (a, b) external_ediv = [%Michelson ({| { UNPAIR ; EDIV } |} : a * b -> (a, b) external_ediv)] (l, r)
#endif

#if UNCURRY
[@private]
  let assert_with_error ((b, s) : bool * string) = [%Michelson ({| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |} : bool * string -> unit)] (b, s)
[@private]
  let assert_some_with_error (type a) ((v, s) : a option * string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |} : a option * string -> unit)] (v, s)
[@private]
  let assert_none_with_error (type a) ((v, s) : a option * string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |} : a option * string -> unit)] (v, s)
[@private]
  let ediv (type a b) ((l, r) : (a * b)) : (a, b) external_u_ediv = [%Michelson ({| { UNPAIR ; EDIV } |} : a * b -> (a, b) external_u_ediv)] (l, r)
#endif



#if TEST_LIB

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

#if CURRY
  let run (type a b) (f : a -> b) (v : a) : michelson_program = [%external ("TEST_RUN", f, v)]
  let eval (type a) (x : a) : michelson_program = run (fun (x : a) -> x) x
#endif

#if UNCURRY
  let run (type a b) ((f, v) : (a -> b) * a) : michelson_program = [%external ("TEST_RUN", f, v)]
  let eval (type a) (x : a) : michelson_program = run ((fun (x : a) -> x) , x)
#endif



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
  let cast_address (type a b) (a : address) : (a, b) typed_address = [%external ("TEST_CAST_ADDRESS", a)]
  let register_delegate (kh : key_hash) : unit = [%external ("TEST_REGISTER_DELEGATE", kh)]
  let register_constant (m : michelson_program) : string = [%external ("TEST_REGISTER_CONSTANT", m)]
  let to_typed_address (type a b) (c : a contract) : (a, b) typed_address = [%external ("TEST_TO_TYPED_ADDRESS", c)]
  let constant_to_michelson_program (s : string) : michelson_program = [%external ("TEST_CONSTANT_TO_MICHELSON", s)]
  let restore_context (u : unit) : unit = [%external ("TEST_POP_CONTEXT", u)]
  let save_context (u : unit) : unit = [%external ("TEST_PUSH_CONTEXT", u)]
  let drop_context (u : unit) : unit = [%external ("TEST_DROP_CONTEXT", u)]
  let to_string (type a) (v : a) : string = [%external ("TEST_TO_STRING", v)]
  let get_storage (type p s) (t : (p, s) typed_address) : s =
    let c : p contract = to_contract t in
    let a : address = [%external ("ADDRESS", c)] in
    let s : michelson_program = get_storage_of_address a in
    (decompile s : s)
  let set_baker_policy (bp : test_baker_policy) : unit = [%external ("TEST_SET_BAKER", bp)]
  let set_baker (a : address) : unit = set_baker_policy (By_account a)
  let size (c : michelson_contract) : int = [%external ("TEST_SIZE", c)]
  let compile_contract (type p s) (f : p * s -> operation list * s) : michelson_contract =
    let ast_c : ast_contract = [%external ("TEST_COMPILE_CONTRACT", f)] in
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

  module PBT = struct
    let gen (type a) : a pbt_gen = [%external ("TEST_RANDOM", false)]
    let gen_small (type a) : a pbt_gen = [%external ("TEST_RANDOM", true)]
#if CURRY
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
#endif
#if UNCURRY
    let make_test (type a) ((g, p) : a pbt_gen * (a -> bool)) : a pbt_test = (g, p)
    let run (type a) (((g, p), k) : a pbt_test * nat) : a pbt_result =
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
#endif
  end

#if CURRY
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
  let create_chest (b : bytes) (n : nat) : chest * chest_key = [%external ("TEST_CREATE_CHEST", b, n)]
  let create_chest_key (c : chest) (n : nat) : chest_key = [%external ("TEST_CREATE_CHEST_KEY", c, n)]
  let transfer_to_contract (type p) (c : p contract) (s : p) (t : tez) : test_exec_result =
    let a : address = [%external ("ADDRESS", c)] in
    let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
    let s : michelson_program = eval s in
    [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", a, e, s, t)]
  let transfer_to_contract_exn (type p) (c : p contract) (s : p) (t : tez) : nat =
      let a : address = [%external ("ADDRESS", c)] in
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
  let originate (type p s) (f : p * s -> operation list * s) (s : s) (t : tez) : ((p, s) typed_address * michelson_contract * int) =
    let f = compile_contract f in
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
#endif

#if UNCURRY
  let get_last_events_from (type a p s) ( (addr,rtag) : (p,s) typed_address * string) : a list =
    let addr = Tezos.address (to_contract addr) in
    let event_map : (address * a) list = [%external ("TEST_LAST_EVENTS", rtag)] in
    let f ((acc, (c_addr,event)) : a list * (address * a)) : a list =
      if addr = c_addr then event::acc
      else acc
    in
    List.fold (f, event_map, ([]: a list))
  let transfer ((a, s, t) : address * michelson_program * tez) : test_exec_result = [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", a, (None : string option), s, t)]
  let transfer_exn ((a, s, t) : address * michelson_program * tez) : nat = [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS_EXN", a, (None : string option), s, t)]
  let log (type a) (v : a) : unit =
    let nl = [%external ("TEST_UNESCAPE_STRING", "\n")] in
    let s = to_string v ^ nl in
    print s
  let reset_state ((n, l) : nat * tez list) : unit = [%external ("TEST_STATE_RESET", (None : timestamp option), n, l)]
  let reset_state_at ((t, n, l) : timestamp * nat * tez list) : unit = [%external ("TEST_STATE_RESET", (Some t), n, l)]
  let bootstrap_contract (type p s) ((f, s, t) : (p * s -> operation list * s) * s * tez) : unit = [%external ("TEST_BOOTSTRAP_CONTRACT", f, s, t)]
  let mutate_value (type a) ((n, v) : nat * a) : (a * mutation) option = [%external ("TEST_MUTATE_VALUE", n, v)]
  let save_mutation ((s, m) : string * mutation) : string option = [%external ("TEST_SAVE_MUTATION", s, m)]
  let sign ((sk,d) : string * bytes) : signature = [%external ("TEST_SIGN", sk, d)]
  let add_account ((s, k) : string * key) : unit = [%external ("TEST_ADD_ACCOUNT", s, k)]
  let baker_account ((p, o) : (string * key) * tez option) : unit = [%external ("TEST_BAKER_ACCOUNT", p, o)]
  let set_big_map (type a b) ((i, m) : int * (a, b) big_map) : unit = [%external ("TEST_SET_BIG_MAP", i, m)]
  let create_chest ((b, n) : bytes * nat) : chest * chest_key = [%external ("TEST_CREATE_CHEST", b, n)]
  let create_chest_key ((c, n) : chest * nat) : chest_key = [%external ("TEST_CREATE_CHEST_KEY", c, n)]
  let transfer_to_contract (type p) ((c, s, t) : p contract * p * tez) : test_exec_result =
      let a : address = [%external ("ADDRESS", c)] in
      let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
      let s : michelson_program = eval s in
      [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", a, e, s, t)]
  let transfer_to_contract_exn (type p) ((c, s, t) : p contract * p * tez) : nat =
      let a : address = [%external ("ADDRESS", c)] in
      let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
      let s : michelson_program = eval s in
      [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS_EXN", a, e, s, t)]
  let michelson_equal ((m1, m2) : michelson_program * michelson_program) : bool = m1 = m2
  let to_entrypoint (type a b c) ((s, t) : string * (a, b) typed_address) : c contract =
    let s = if String.length s > 0n then
              if String.sub (0n, 1n, s) = "%" then
                let () = log "WARNING: Test.to_entrypoint: automatically removing starting %" in
                String.sub (1n, (abs (String.length s - 1)), s)
	      else s
	    else s in
    [%external ("TEST_TO_ENTRYPOINT", s, t)]
  let originate_contract ((c, s, t) : michelson_contract * michelson_program * tez) : address = [%external ("TEST_ORIGINATE", c, s, t)]
  let originate (type p s) ((f, s, t) : (p * s -> operation list * s) * s * tez) : ((p, s) typed_address * michelson_contract * int) =
    let f = compile_contract f in
    let s = eval s in
    let a = originate_contract (f, s, t) in
    let c = size f in
    let a : (p, s) typed_address = cast_address a in
    (a, f, c)
  let compile_contract_from_file ((fn, e, v) : string * string * string list) : michelson_contract =
    let ast_c : ast_contract = [%external ("TEST_COMPILE_CONTRACT_FROM_FILE", fn, e, v, (None : nat option))] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]
  let originate_from_file ((fn, e, v, s, t) : string * string * string list * michelson_program * tez) : address * michelson_contract * int =
    let f = compile_contract_from_file (fn, e, v) in
    let a = originate_contract (f, s, t) in
    let c = size f in
    (a, f, c)
  let mutation_test (type a b) ((v, tester) : a * (a -> b)) : (b * mutation) option =
    let try_with (type a) (v : unit -> a) (c : unit -> a) = [%external ("TEST_TRY_WITH", v, c)] in
    type ret_code = Passed of (b * mutation) | Continue | Stop in
    let rec mutation_nth (n : nat) : (b * mutation) option =
      let curr = match mutate_value (n, v) with
        | Some (v, m) -> try_with (fun (_ : unit) -> let b = tester v in Passed (b, m)) (fun (_ : unit) -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> None
      | Continue -> mutation_nth (n + 1n)
      | Passed (b, m) -> Some (b, m) in
    mutation_nth 0n
  let mutation_test_all (type a b) ((v, tester) : a * (a -> b)) : (b * mutation) list =
    let try_with (type a) (v : unit -> a) (c : unit -> a) = [%external ("TEST_TRY_WITH", v, c)] in
    type ret_code = Passed of (b * mutation) | Continue | Stop in
    let rec mutation_nth (acc : (b * mutation) list) (n : nat) : (b * mutation) list =
      let curr = match mutate_value (n, v) with
        | Some (v, m) -> try_with (fun (_ : unit) -> let b = tester v in Passed (b, m)) (fun (_ : unit) -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> acc
      | Continue -> mutation_nth acc (n + 1n)
      | Passed (b, m) -> mutation_nth ((b, m) :: acc) (n + 1n) in
    mutation_nth ([] : (b * mutation) list) 0n
  let originate_from_file_and_mutate (type b) ((fn, e, v, s, t, tester) : string * string * string list * michelson_program * tez * (address * michelson_contract * int -> b)) : (b * mutation) option =
    let wrap_tester (v : ast_contract) : b =
      let f = [%external ("TEST_COMPILE_AST_CONTRACT", v)] in
      let a = originate_contract (f, s, t) in
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
  let originate_from_file_and_mutate_all (type b) ((fn, e, v, s, t, tester) : string * string * string list * michelson_program * tez * (address * michelson_contract * int -> b)) : (b * mutation) list =
    let wrap_tester (v : ast_contract) : b =
      let f = [%external ("TEST_COMPILE_AST_CONTRACT", v)] in
      let a = originate_contract (f, s, t) in
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
#endif

end

#else

(* TEST STUBS *)
type ('a, 'b) typed_address = unit
type michelson_program = unit
type test_exec_result = unit
type michelson_contract = unit
type mutation = unit
type test_baker_policy = unit
type 'a pbt_gen = unit
type 'a pbt_test = unit
type 'a pbt_result = unit

module Test = struct

[@private] let stub (type a b) (x:a) : b = [%Michelson ({|{ FAILWITH }|} : a -> b)] x

#if CURRY
  let run (type a b) (_f : a -> b) (_v : a) : michelson_program = stub ()
  let eval (type a) (_x : a) : michelson_program = stub ()
#endif

#if UNCURRY
  let run (type a b) ((_f, _v) : (a -> b) * a) : michelson_program = stub ()
  let eval (type a) (_x : a) : michelson_program = stub ()
#endif

  let compile_value (type a) (_x : a) : michelson_program = stub ()
  let get_total_voting_power (_u : unit) : nat = stub ()
  let failwith (type a b) (_v : a) : b = stub ()
  let to_contract (type p s) (_t : (p, s) typed_address) : p contract = stub ()
  let set_source (_a : address) : unit = stub ()
  let get_storage_of_address (_a : address) : michelson_program = stub ()
  let get_balance (_a : address) : tez = stub ()
  let print (_v : string) : unit = stub ()
  let eprint (_v : string) : unit = stub ()
  let get_voting_power (_kh : key_hash) : nat = stub ()
  let nth_bootstrap_contract (_i : nat) : address = stub ()
  let nth_bootstrap_account (_i : int) : address = stub ()
  let get_bootstrap_account (_n : nat) : address * key * string = stub ()
  let nth_bootstrap_typed_address (type a b) (_n : nat) : (a, b) typed_address = stub ()
  let last_originations (_u : unit) : (address, address list) map = stub ()
  let random (type a) (_u : unit) : a = stub ()
  let new_account (_u : unit) : string * key = stub ()
  let decompile (type a) (_m : michelson_program) : a = stub ()
  let bake_until_n_cycle_end (_n : nat) : unit = stub ()
  let cast_address (type a b) (_a : address) : (a, b) typed_address = stub ()
  let register_delegate (_kh : key_hash) : unit = stub ()
  let register_constant (_m : michelson_program) : string = stub ()
  let to_typed_address (type a b) (_c : a contract) : (a, b) typed_address = stub ()
  let constant_to_michelson_program (_s : string) : michelson_program = stub ()
  let restore_context (_u : unit) : unit = stub ()
  let save_context (_u : unit) : unit = stub ()
  let drop_context (_u : unit) : unit = stub ()
  let to_string (type a) (_v : a) : string = stub ()
  let get_storage (type p s) (_t : (p, s) typed_address) : s = stub ()
  let set_baker_policy (_bp : test_baker_policy) : unit = stub ()
  let set_baker (_a : address) : unit = stub ()
  let size (_c : michelson_contract) : int = stub ()
  let compile_contract (type p s) (_f : p * s -> operation list * s) : michelson_contract = stub ()
  let read_contract_from_file (_fn : string) : michelson_contract = stub ()
  let chr (_n : nat) : string option = stub ()
  let nl : string = "NEWLINE"
  let println (_v : string) : unit = stub ()

#if CURRY
  let transfer (_a : address) (_s : michelson_program) (_t : tez) : test_exec_result = stub ()
  let transfer_exn (_a : address) (_s : michelson_program) (_t : tez) : nat = stub ()
  let log (type a) (_v : a) : unit = stub ()
  let reset_state (_n : nat) (_l : tez list) : unit = stub ()
  let reset_state_at (_t:timestamp) (_n : nat) (_l : tez list) : unit = stub ()
  let bootstrap_contract (type p s) (_f : p * s -> operation list * s) (_s : s) (_t : tez) : unit = stub ()
  let mutate_value (type a) (_n : nat) (_v : a) : (a * mutation) option = stub ()
  let save_mutation (_s : string) (_m : mutation) : string option = stub ()
  let mutation_test (type a b) (_v : a) (_f : a -> b) : (b * mutation) option = stub ()
  let mutation_test_all (type a b) (_v : a) (_f : a -> b) : (b * mutation) list = stub ()
  let sign (_sk : string) (_d : bytes) : signature = stub ()
  let add_account (_s : string) (_k : key) : unit = stub ()
  let baker_account (_p : string * key) (_o : tez option) : unit = stub ()
  let set_big_map (type a b) (_i : int) (_m : (a, b) big_map) : unit = stub ()
  let create_chest (_b : bytes) (_n : nat) : chest * chest_key = stub ()
  let create_chest_key (_c : chest) (_n : nat) : chest_key = stub ()
  let transfer_to_contract (type p) (_c : p contract) (_s : p) (_t : tez) : test_exec_result = stub ()
  let transfer_to_contract_exn (type p) (_c : p contract) (_s : p) (_t : tez) : nat = stub ()
  let michelson_equal (_m1 : michelson_program) (_m2 : michelson_program) : bool = stub ()
  let to_entrypoint (type a b c) (_s : string) (_t : (a, b) typed_address) : c contract = stub ()
  let originate_contract (_c : michelson_contract) (_s : michelson_program) (_t : tez) : address = stub ()
  let originate (type p s) (_f : p * s -> operation list * s) (_s : s) (_t : tez) : ((p, s) typed_address * michelson_contract * int) = stub ()
  let compile_contract_from_file (_fn : string) (_e : string) (_v : string list) : michelson_contract = stub ()
  let originate_from_file (_fn : string) (_e : string) (_v : string list) (_s : michelson_program) (_t : tez) : address * michelson_contract * int = stub ()
#endif

#if UNCURRY
  let transfer ((_a, _s, _t) : address * michelson_program * tez) : test_exec_result = stub ()
  let transfer_exn ((_a, _s, _t) : address * michelson_program * tez) : nat = stub ()
  let log (type a) (_v : a) : unit = stub ()
  let reset_state ((_n, _l) : nat * tez list) : unit = stub ()
  let reset_state_at ((_t, _n, _l) : timestamp * nat * tez list) : unit = stub ()
  let bootstrap_contract (type p s) ((_f, _s, _t) : (p * s -> operation list * s) * s * tez) : unit = stub ()
  let mutate_value (type a) ((_n, _v) : nat * a) : (a * mutation) option = stub ()
  let save_mutation ((_s, _m) : string * mutation) : string option = stub ()
  let mutation_test (type a b) ((_v, _f) : a * (a -> b)) : (b * mutation) option = stub ()
  let mutation_test_all (type a b) ((_v, _f) : a * (a -> b)) : (b * mutation) list = stub ()
  let sign ((_sk,_d) : string * bytes) : signature = stub ()
  let add_account ((_s, _k) : string * key) : unit = stub ()
  let baker_account ((_p, _o) : (string * key) * tez option) : unit = stub ()
  let set_big_map (type a b) ((_i, _m) : int * (a, b) big_map) : unit = stub ()
  let create_chest ((_b, _n) : bytes * nat) : chest * chest_key = stub ()
  let create_chest_key ((_c, _n) : chest * nat) : chest_key = stub ()
  let transfer_to_contract (type p) ((_c, _s, _t) : p contract * p * tez) : test_exec_result = stub ()
  let transfer_to_contract_exn (type p) ((_c, _s, _t) : p contract * p * tez) : nat = stub ()
  let michelson_equal ((_m1, _m2) : michelson_program * michelson_program) : bool = stub ()
  let to_entrypoint (type a b c) ((_s, _t) : string * (a, b) typed_address) : c contract = stub ()
  let originate_contract ((_c, _s, _t) : michelson_contract * michelson_program * tez) : address = stub ()
  let originate (type p s) ((_f, _s, _t) : (p * s -> operation list * s) * s * tez) : ((p, s) typed_address * michelson_contract * int) = stub ()
  let compile_contract_from_file ((_fn, _e, _v) : string * string * string list) : michelson_contract = stub ()
  let originate_from_file ((_fn, _e, _v, _s, _t) : string * string * string list * michelson_program * tez) : address * michelson_contract * int = stub ()
#endif

end

#endif
