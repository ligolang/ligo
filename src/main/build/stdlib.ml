let lib (s : Syntax_types.t) =
  match s with
  | PascaLIGO _ | ReasonLIGO | JsLIGO ->"
module Tezos = struct
  [@hidden] [@inline] let get_contract (type a) (a : address) : (a contract) = [%external \"CONTRACT\"] a
  [@hidden] [@inline] let get_contract_opt (type a) (a : address) : (a contract) option = [%external \"CONTRACT_OPT\"] a
  [@hidden] [@inline] let get_contract_with_error (type a) ((a, s) : address * string) : a contract = [%external \"CONTRACT_WITH_ERROR\"] a s
  (* [@hidden] [@inline] let get_entrypoint_opt (type a) ((s, a) : string * address) : (a contract) option = [%external \"CONTRACT_ENTRYPOINT_OPT\"] s a *)
  [@thunk] [@hidden] [@inline] let balance : tez = [%Michelson ({| { DROP ; BALANCE } |} : unit -> tez)] ()
  [@thunk] [@hidden] [@inline] let amount : tez = [%Michelson ({| { DROP ; AMOUNT } |} : unit -> tez)] ()
  [@thunk] [@hidden] [@inline] let now : timestamp = [%Michelson ({| { DROP ; NOW } |} : unit -> timestamp)] ()
  [@thunk] [@hidden] [@inline] let sender : address = [%Michelson ({| { DROP ; SENDER } |} : unit -> address)] ()
  [@thunk] [@hidden] [@inline] let source : address = [%Michelson ({| { DROP ; SOURCE } |} : unit -> address)] ()
  [@thunk] [@hidden] [@inline] let level : nat = [%Michelson ({| { DROP ; LEVEL } |} : unit -> nat)] ()
  [@thunk] [@hidden] [@inline] let self_address : address = [%external \"SELF_ADDRESS\"]
  [@thunk] [@hidden] [@inline] let chain_id : chain_id = [%Michelson ({| { DROP ; CHAIN_ID } |} : unit -> chain_id)] ()
  [@thunk] [@hidden] [@inline] let total_voting_power : nat = [%Michelson ({| { DROP ; TOTAL_VOTING_POWER } |} : unit -> nat)] ()
  (* [@thunk] [@hidden] [@inline] let self (type a) (s : string) : a contract = [%external \"SELF\"] s *)
  [@hidden] [@inline] let voting_power (kh : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat)] kh
  [@hidden] [@inline] let address (type a) (c : a contract) : address = [%external \"ADDRESS\"] c
  [@hidden] [@inline] let implicit_account (kh : key_hash) : unit contract = [%external \"IMPLICIT_ACCOUNT\"] kh
  [@hidden] [@inline] let create_ticket (type a) ((v, n) : a * nat) : a ticket = [%Michelson ({| { UNPAIR ; TICKET } |} : a * nat -> a ticket)] (v, n)
  [@hidden] [@inline] let join_tickets (type a) (t : a ticket * a ticket) : (a ticket) option = [%Michelson ({| { JOIN_TICKETS } |} : a ticket * a ticket -> a ticket option)] t
  [@hidden] [@inline] let split_ticket (type a) ((t, p) : (a ticket) * (nat * nat)) : (a ticket * a ticket) option = [%Michelson ({| { UNPAIR ; SPLIT_TICKET } |} : a ticket * (nat * nat) -> (a ticket * a ticket) option)] (t, p)
  [@hidden] [@inline] let read_ticket (type a) (t : a ticket) : (address * (a * nat)) * a ticket = [%Michelson ({| { READ_TICKET ; PAIR } |} : a ticket -> (address * (a * nat)) * a ticket)] t
  (* [@hidden] [@inline] let create_contract (type a b) ((c, kh, mu, s) : (a * b -> operation list * b) * key_hash * tez * b) : operation * address = [%external \"CREATE_CONTRACT\"] c kh mu s *)
  [@hidden] [@inline] let never (type a) (n : never) : a = [%Michelson ({| { NEVER } |} : never -> a)] n
  [@hidden] [@inline] let pairing_check (l : (bls12_381_g1 * bls12_381_g2) list) : bool = [%Michelson ({| { PAIRING_CHECK } |} : (bls12_381_g1 * bls12_381_g2) list -> bool)] l
  [@hidden] [@inline] let open_chest ((ck, c, n) : chest_key * chest * nat) : chest_opening_result = [%external \"OPEN_CHEST\"] ck c n
  [@hidden] [@inline] let call_view (type a b) ((s, x, a) : string * a * address)  : b option = [%external \"VIEW\"] s x a
  [@hidden] [@inline] let constant (type a) (s : string) : a = [%external \"GLOBAL_CONSTANT\"] s
  [@hidden] [@inline] let transaction (type a) ((a, mu, c) : a * tez * a contract) : operation = [%external \"CALL\"] a mu c
  [@hidden] [@inline] let set_delegate (o : key_hash option) : operation = [%external \"SET_DELEGATE\"] o
end
module Bitwise = struct
  (* [@hidden] [@inline] let and (type a b) ((l, r) : (a, b)) : (a, b) external_and = [%external \"AND\"] l r *)
  [@hidden] [@inline] let xor ((l, r) : nat * nat) : nat = [%external \"XOR\"] l r
  (* [@hidden] [@inline] let or ((l, r) : nat * nat) : nat = [%external \"OR\"] l r *)
  [@hidden] [@inline] let shift_left ((l, r) : nat * nat) : nat = [%external \"LSL\"] l r
  [@hidden] [@inline] let shift_right ((l, r) : nat * nat) : nat = [%external \"LSR\"] l r
end
module Big_map = struct
  [@hidden] [@inline] let empty (type k v) : (k, v) big_map = [%external \"BIG_MAP_EMPTY\"]
  [@hidden] [@inline] let mem (type k v) ((k, m) : k * (k, v) big_map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) big_map -> bool)] (k, m)
  [@hidden] [@inline] let add (type k v) ((k, v, m) : k * v * (k, v) big_map) : (k, v) big_map = [%external \"MAP_ADD\"] k v m
  [@hidden] [@inline] let remove (type k v) ((k, m) : k * (k, v) big_map) : (k, v) big_map = [%external \"MAP_REMOVE\"] k m
  [@hidden] [@inline] let update (type k v) ((k, v, m) : k * v option * (k, v) big_map) : (k, v) big_map = [%external \"MAP_UPDATE\"] k v m
  [@hidden] [@inline] let get_and_update (type k v) ((k, v, m) : k * v option * (k, v) big_map) : v option * (k, v) big_map = [%external \"BIG_MAP_GET_AND_UPDATE\"] k v m
  [@hidden] [@inline] let find_opt (type k v) ((k, m) : k * (k, v) big_map) : v option = [%external \"MAP_FIND_OPT\"] k m
  [@hidden] [@inline] let find (type k v) ((k, m) : k * (k, v) big_map) : v = [%external \"MAP_FIND\"] k m
end
module Map = struct
  [@hidden] [@inline] let empty (type k v) : (k, v) map = [%external \"MAP_EMPTY\"]
  [@hidden] [@inline] let size (type k v) (m : (k, v) map) : nat = [%Michelson ({| { SIZE } |} : (k, v) map -> nat)] m
  [@hidden] [@inline] let mem (type k v) ((k, m) : k * (k, v) map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) map -> bool)] (k, m)
  [@hidden] [@inline] let add (type k v) ((k, v, m) : k * v * (k, v) map) : (k, v) map = [%external \"MAP_ADD\"] k v m
  [@hidden] [@inline] let remove (type k v) ((k, m) : k * (k, v) map) : (k, v) map = [%external \"MAP_REMOVE\"] k m
  [@hidden] [@inline] let update (type k v) ((k, v, m) : k * v option * (k, v) map) : (k, v) map = [%external \"MAP_UPDATE\"] k v m
  [@hidden] [@inline] let get_and_update (type k v) ((k, v, m) : k * v option * (k, v) map) : v option * (k, v) map = [%external \"MAP_GET_AND_UPDATE\"] k v m
  [@hidden] [@inline] let find_opt (type k v) ((k, m) : k * (k, v) map) : v option = [%external \"MAP_FIND_OPT\"] k m
  [@hidden] [@inline] let find (type k v) ((k, m) : k * (k, v) map) : v = [%external \"MAP_FIND\"] k m
  [@hidden] [@inline] let iter (type k v) ((f, m) : (k * v -> unit) * (k, v) map) : unit = [%external \"MAP_ITER\"] f m
  [@hidden] [@inline] let map (type k v w) ((f, m) : (k * v -> w) * (k, v) map) : (k, w) map = [%external \"MAP_MAP\"] f m
  [@hidden] [@inline] let fold (type k v c) ((f, m, i) : (c * (k * v) -> c) * (k, v) map * c) : c = [%external \"MAP_FOLD\"] f m i
end
module Set = struct
  [@hidden] [@inline] let empty (type a) : a set = [%external \"SET_EMPTY\"]
  [@hidden] [@inline] let mem (type a) ((x, s) : a * a set) : bool = [%external \"SET_MEM\"] x s
  [@hidden] [@inline] let add (type a) ((x, s) : a * a set) : a set = [%external \"SET_ADD\"] x s
  [@hidden] [@inline] let remove (type a) ((x, s) : a * a set) : a set = [%external \"SET_REMOVE\"] x s
  [@hidden] [@inline] let update (type a) ((x, b, s) : a * bool * a set) = [%external \"SET_UPDATE\"] x b s
  [@hidden] [@inline] let iter (type a) ((f, s) : (a -> unit) * a set) : unit = [%external \"SET_ITER\"] f s
  [@hidden] [@inline] let fold (type a b) ((f, s, i) : (b * a -> b) * a set * b) : b = [%external \"SET_FOLD\"] f s i
  [@hidden] [@inline] let fold_desc (type a b) ((f, s, i) : (a * b -> b) * a set * b) : b = [%external \"SET_FOLD_DESC\"] f s i
  [@hidden] [@inline] let size (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)]  s
  [@hidden] [@inline] let cardinal (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)]  s
end
module List = struct
  [@hidden] [@inline] let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  [@hidden] [@inline] let size (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  [@hidden] [@inline] let head_opt (type a) (xs : a list) : a option = [%external \"LIST_HEAD_OPT\"] xs
  [@hidden] [@inline] let tail_opt (type a) (xs : a list) : (a list) option = [%external \"LIST_TAIL_OPT\"] xs
  [@hidden] [@inline] let map (type a b) ((f, xs) : (a -> b) * a list) : b list = [%external \"LIST_MAP\"] f xs
  [@hidden] [@inline] let iter (type a) ((f, xs) : (a -> unit) * a list): unit = [%external \"LIST_ITER\"] f xs
  [@hidden] [@inline] let fold (type a b) ((f, xs, i) : (b * a -> b) * a list * b) : b = [%external \"LIST_FOLD\"] f xs i
  [@hidden] [@inline] let fold_left (type a b) ((f, i, xs) : (b * a -> b) * b * a list) : b = [%external \"LIST_FOLD_LEFT\"] f i xs
  [@hidden] [@inline] let fold_right (type a b) ((f, xs, i) : (a * b -> b) * a list * b) : b = [%external \"LIST_FOLD_RIGHT\"] f xs i
  [@hidden] [@inline] let cons (type a) ((x, xs) : a * a list) : a list = [%external \"CONS\"] x xs
end
module String = struct
  [@hidden] [@inline] let concat ((b1, b2) : string * string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] (b1, b2)
  [@hidden] [@inline] let sub ((s, l, b) : nat * nat * string) : string = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] (s, l, b)
  [@hidden] [@inline] let length (b : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] b
end
module Option = struct
  [@hidden] [@inline] let unopt (type a) (v : a option) : a = [%external \"UNOPT\"] v
  [@hidden] [@inline] let unopt_with_error (type a) ((v, s) : (a option) * string) : a = [%external \"UNOPT_WITH_ERROR\"] v s
  (* [@hidden] [@inline] let map (type a b) ((f, v) : (a -> b) * (a option)) : b option = [%external \"OPTION_MAP\"] f v *)
end
module Bytes = struct
  [@hidden] [@inline] let concat ((b1, b2) : bytes * bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] (b1, b2)
  [@hidden] [@inline] let sub ((s, l, b) : nat * nat * bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (s, l, b)
  [@hidden] [@inline] let pack (type a) (v : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] v
  [@hidden] [@inline] let unpack (type a) (b : bytes) : a option = [%external \"BYTES_UNPACK\"] b
  [@hidden] [@inline] let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b
end
module Crypto = struct
  [@hidden] [@inline] let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
  [@hidden] [@inline] let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k
  [@hidden] [@inline] let check ((k, s, b) : key * signature * bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end
[@private] [@hidden] [@inline] let assert (b : bool) : unit = [%Michelson ({| { IF { UNIT } { PUSH string \"failed assertion\" ; FAILWITH } } |} : bool -> unit)] b
[@private] [@hidden] [@inline] let assert_with_error ((b, s) : bool * string) = [%Michelson ({| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |} : bool * string -> unit)] (b, s)
[@private] [@hidden] [@inline] let assert_some (type a) (v : a option) : unit = [%Michelson ({| { IF_NONE { PUSH string \"failed assert some\" ; FAILWITH } { DROP ; UNIT } } |} : a option -> unit)] v
[@private] [@hidden] [@inline] let assert_some_with_error (type a) ((v, s) : a option * string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |} : a option * string -> unit)] (v, s)
[@private] [@hidden] [@inline] let assert_none (type a) (v : a option) : unit = [%Michelson ({| { IF_NONE { UNIT } { PUSH string \"failed assert none\" ; FAILWITH } } |} : a option -> unit)] v
[@private] [@hidden] [@inline] let assert_none_with_error (type a) ((v, s) : a option * string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |} : a option * string -> unit)] (v, s)
[@private] [@hidden] [@inline] let abs (i : int) : nat = [%Michelson ({| { ABS } |} : int -> nat)] i
[@private] [@hidden] [@inline] let is_nat (i : int) : nat option = [%Michelson ({| { ISNAT } |} : int -> nat option)] i
[@private] [@hidden] [@inline] let true : bool = [%external \"TRUE\"]
[@private] [@hidden] [@inline] let false : bool = [%external \"FALSE\"]
[@private] [@hidden] [@inline] let unit : unit = [%external \"UNIT\"]
[@private] [@hidden] [@inline] let failwith (type a) (v : a) : a external_failwith = [%external \"FAILWITH\"] v
[@private] [@hidden] [@inline] let int (type a) (v : a) : a external_int = [%Michelson ({| { INT } |} : a -> a external_int)] v
[@private] [@hidden] [@inline] let ediv (type a b) ((l, r) : (a * b)) : (a, b) external_u_ediv = [%Michelson ({| { UNPAIR ; EDIV } |} : a * b -> (a, b) external_u_ediv)] (l, r)
"
  | CameLIGO -> "
module Tezos = struct
  [@hidden] [@inline] let get_contract (type a) (a : address) : (a contract) = [%external \"CONTRACT\"] a
  [@hidden] [@inline] let get_contract_opt (type a) (a : address) : (a contract) option = [%external \"CONTRACT_OPT\"] a
  [@hidden] [@inline] let get_contract_with_error (type a) (a : address) (s : string) : a contract = [%external \"CONTRACT_WITH_ERROR\"] a s
  (* [@hidden] [@inline] let get_entrypoint_opt (type a) (s : string) (a : address) : (a contract) option = [%external \"CONTRACT_ENTRYPOINT_OPT\"] s a *)
  [@thunk] [@hidden] [@inline] let balance : tez = [%Michelson ({| { DROP ; BALANCE } |} : unit -> tez)] ()
  [@thunk] [@hidden] [@inline] let amount : tez = [%Michelson ({| { DROP ; AMOUNT } |} : unit -> tez)] ()
  [@thunk] [@hidden] [@inline] let now : timestamp = [%Michelson ({| { DROP ; NOW } |} : unit -> timestamp)] ()
  [@thunk] [@hidden] [@inline] let sender : address = [%Michelson ({| { DROP ; SENDER } |} : unit -> address)] ()
  [@thunk] [@hidden] [@inline] let source : address = [%Michelson ({| { DROP ; SOURCE } |} : unit -> address)] ()
  [@thunk] [@hidden] [@inline] let level : nat = [%Michelson ({| { DROP ; LEVEL } |} : unit -> nat)] ()
  [@thunk] [@hidden] [@inline] let self_address : address = [%external \"SELF_ADDRESS\"]
  [@thunk] [@hidden] [@inline] let chain_id : chain_id = [%Michelson ({| { DROP ; CHAIN_ID } |} : unit -> chain_id)] ()
  [@thunk] [@hidden] [@inline] let total_voting_power : nat = [%Michelson ({| { DROP ; TOTAL_VOTING_POWER } |} : unit -> nat)] ()
  (* [@thunk] [@hidden] [@inline] let self (type a) (s : string) : a contract = [%external \"SELF\"] s *)
  [@hidden] [@inline] let voting_power (kh : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat)] kh
  [@hidden] [@inline] let address (type a) (c : a contract) : address = [%external \"ADDRESS\"] c
  [@hidden] [@inline] let implicit_account (kh : key_hash) : unit contract = [%external \"IMPLICIT_ACCOUNT\"] kh
  [@hidden] [@inline] let create_ticket (type a) (v : a) (n : nat) : a ticket = [%Michelson ({| { UNPAIR ; TICKET } |} : a * nat -> a ticket)] (v, n)
  [@hidden] [@inline] let join_tickets (type a) (t : a ticket * a ticket) : (a ticket) option = [%Michelson ({| { JOIN_TICKETS } |} : a ticket * a ticket -> a ticket option)] t
  [@hidden] [@inline] let split_ticket (type a) (t : a ticket) (p : nat * nat) : (a ticket * a ticket) option = [%Michelson ({| { UNPAIR ; SPLIT_TICKET } |} : a ticket * (nat * nat) -> (a ticket * a ticket) option)] (t, p)
  [@hidden] [@inline] let read_ticket (type a) (t : a ticket) : (address * (a * nat)) * a ticket = [%Michelson ({| { READ_TICKET ; PAIR } |} : a ticket -> (address * (a * nat)) * a ticket)] t
  (* [@hidden] [@inline] let create_contract (type a b) (c : a * b -> operation list * b) (kh : key_hash) (mu : tez) (s : b) : operation * address = [%external \"CREATE_CONTRACT\"] c kh mu s *)
  [@hidden] [@inline] let never (type a) (n : never) : a = [%Michelson ({| { NEVER } |} : never -> a)] n
  [@hidden] [@inline] let pairing_check (l : (bls12_381_g1 * bls12_381_g2) list) : bool = [%Michelson ({| { PAIRING_CHECK } |} : (bls12_381_g1 * bls12_381_g2) list -> bool)] l
  [@hidden] [@inline] let open_chest (ck : chest_key) (c : chest) (n : nat) : chest_opening_result = [%external \"OPEN_CHEST\"] ck c n
  [@hidden] [@inline] let call_view (type a b) (s : string) (x : a) (a : address)  : b option = [%external \"VIEW\"] s x a
  [@hidden] [@inline] let constant (type a) (s : string) : a = [%external \"GLOBAL_CONSTANT\"] s
  [@hidden] [@inline] let transaction (type a) (a : a) (mu : tez) (c : a contract) : operation = [%external \"CALL\"] a mu c
  [@hidden] [@inline] let set_delegate (o : key_hash option) : operation = [%external \"SET_DELEGATE\"] o
end
module Bitwise = struct
  (* [@hidden] [@inline] let and (type a b) (l : a) (r : b) : (a, b) external_and = [%external \"AND\"] l r *)
  [@hidden] [@inline] let xor (l : nat) (r : nat) : nat = [%external \"XOR\"] l r
  (* [@hidden] [@inline] let or (l : nat) (r : nat) : nat = [%external \"OR\"] l r *)
  [@hidden] [@inline] let shift_left (l : nat) (r : nat) : nat = [%external \"LSL\"] l r
  [@hidden] [@inline] let shift_right (l : nat) (r : nat) : nat = [%external \"LSR\"] l r
end
module Big_map = struct
  [@hidden] [@inline] let empty (type k v) : (k, v) big_map = [%external \"BIG_MAP_EMPTY\"]
  [@hidden] [@inline] let mem (type k v) (k : k) (m : (k, v) big_map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) big_map -> bool)] (k, m)
  [@hidden] [@inline] let add (type k v) (k : k) (v : v) (m : (k, v) big_map) : (k, v) big_map = [%external \"MAP_ADD\"] k v m
  [@hidden] [@inline] let remove (type k v) (k : k) (m : (k, v) big_map) : (k, v) big_map = [%external \"MAP_REMOVE\"] k m
  [@hidden] [@inline] let update (type k v) (k : k) (v : v option) (m : (k, v) big_map) : (k, v) big_map = [%external \"MAP_UPDATE\"] k v m
  [@hidden] [@inline] let get_and_update (type k v) (k : k) (v : v option) (m : (k, v) big_map) : v option * (k, v) big_map = [%external \"BIG_MAP_GET_AND_UPDATE\"] k v m
  [@hidden] [@inline] let find_opt (type k v) (k : k) (m : (k, v) big_map) : v option = [%external \"MAP_FIND_OPT\"] k m
  [@hidden] [@inline] let find (type k v) (k : k) (m : (k, v) big_map) : v = [%external \"MAP_FIND\"] k m
end
module Map = struct
  [@hidden] [@inline] let empty (type k v) : (k, v) map = [%external \"MAP_EMPTY\"]
  [@hidden] [@inline] let size (type k v) (m : (k, v) map) : nat = [%Michelson ({| { SIZE } |} : (k, v) map -> nat)] m
  [@hidden] [@inline] let mem (type k v) (k : k) (m : (k, v) map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) map -> bool)] (k, m)
  [@hidden] [@inline] let add (type k v) (k : k) (v : v) (m : (k, v) map) : (k, v) map = [%external \"MAP_ADD\"] k v m
  [@hidden] [@inline] let remove (type k v) (k : k) (m : (k, v) map) : (k, v) map = [%external \"MAP_REMOVE\"] k m
  [@hidden] [@inline] let update (type k v) (k : k) (v : v option) (m : (k, v) map) : (k, v) map = [%external \"MAP_UPDATE\"] k v m
  [@hidden] [@inline] let get_and_update (type k v) (k : k) (v : v option) (m : (k, v) map) : v option * (k, v) map = [%external \"MAP_GET_AND_UPDATE\"] k v m
  [@hidden] [@inline] let find (type k v) (k : k) (m : (k, v) map) : v = [%external \"MAP_FIND\"] k m
  [@hidden] [@inline] let find_opt (type k v) (k : k) (m : (k, v) map) : v option = [%external \"MAP_FIND_OPT\"] k m
  [@hidden] [@inline] let iter (type k v) (f : k * v -> unit) (m : (k, v) map) : unit = [%external \"MAP_ITER\"] f m
  [@hidden] [@inline] let map (type k v w) (f : k * v -> w) (m : (k, v) map) : (k, w) map = [%external \"MAP_MAP\"] f m
  [@hidden] [@inline] let fold (type k v c) (f : c * (k * v) -> c) (m : (k, v) map) (i : c) : c = [%external \"MAP_FOLD\"] f m i
end
module Set = struct
  [@hidden] [@inline] let empty (type a) : a set = [%external \"SET_EMPTY\"]
  [@hidden] [@inline] let mem (type a) (x : a) (s : a set) : bool = [%external \"SET_MEM\"] x s
  [@hidden] [@inline] let add (type a) (x : a) (s : a set) : a set = [%external \"SET_ADD\"] x s
  [@hidden] [@inline] let remove (type a) (x : a) (s : a set) : a set = [%external \"SET_REMOVE\"] x s
  [@hidden] [@inline] let update (type a) (x : a) (b : bool) (s : a set) = [%external \"SET_UPDATE\"] x b s
  [@hidden] [@inline] let iter (type a) (f : a -> unit) (s : a set) : unit = [%external \"SET_ITER\"] f s
  [@hidden] [@inline] let fold (type a b) (f : b * a -> b) (s : a set) (i : b) : b = [%external \"SET_FOLD\"] f s i
  [@hidden] [@inline] let fold_desc (type a b) (f : a * b -> b) (s : a set) (i : b) : b = [%external \"SET_FOLD_DESC\"] f s i
  [@hidden] [@inline] let size (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)]  s
  [@hidden] [@inline] let cardinal (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)] s
end
module List = struct
  [@hidden] [@inline] let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  [@hidden] [@inline] let size (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  [@hidden] [@inline] let head_opt (type a) (xs : a list) : a option = [%external \"LIST_HEAD_OPT\"] xs
  [@hidden] [@inline] let tail_opt (type a) (xs : a list) : (a list) option = [%external \"LIST_TAIL_OPT\"] xs
  [@hidden] [@inline] let map (type a b) (f : a -> b) (xs : a list) : b list = [%external \"LIST_MAP\"] f xs
  [@hidden] [@inline] let iter (type a) (f : a -> unit) (xs : a list): unit = [%external \"LIST_ITER\"] f xs
  [@hidden] [@inline] let fold (type a b) (f : b * a -> b) (xs : a list) (i : b) : b = [%external \"LIST_FOLD\"] f xs i
  [@hidden] [@inline] let fold_left (type a b) (f : b * a -> b) (i : b) (xs : a list) : b = [%external \"LIST_FOLD_LEFT\"] f i xs
  [@hidden] [@inline] let fold_right (type a b) (f : a * b -> b) (xs : a list) (i : b) : b = [%external \"LIST_FOLD_RIGHT\"] f xs i
  [@hidden] [@inline] let cons (type a) (x : a) (xs : a list) : a list = [%external \"CONS\"] x xs
end
module String = struct
  [@hidden] [@inline] let concat (b1 : string) (b2 : string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] (b1, b2)
  [@hidden] [@inline] let sub (s : nat) (l : nat) (b : string) : string = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] (s, l, b)
  [@hidden] [@inline] let length (b : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] b
end
module Option = struct
  [@hidden] [@inline] let unopt (type a) (v : a option) : a = [%external \"UNOPT\"] v
  [@hidden] [@inline] let unopt_with_error (type a) (v : a option) (s : string) : a = [%external \"UNOPT_WITH_ERROR\"] v s
  (* [@hidden] [@inline] let map (type a b) (f : a -> b) (v : a option) : b option = [%external \"OPTION_MAP\"] f v *)
end
module Bytes = struct
  [@hidden] [@inline] let concat (b1 : bytes) (b2 : bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] (b1, b2)
  [@hidden] [@inline] let sub (s : nat) (l : nat) (b : bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (s, l, b)
  [@hidden] [@inline] let pack (type a) (v : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] v
  [@hidden] [@inline] let unpack (type a) (b : bytes) : a option = [%external \"BYTES_UNPACK\"] b
  [@hidden] [@inline] let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b
end
module Crypto = struct
  [@hidden] [@inline] let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
  [@hidden] [@inline] let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k
  [@hidden] [@inline] let check (k : key) (s : signature) (b : bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end
[@private] [@hidden] [@inline] let assert (b : bool) : unit = [%Michelson ({| { IF { UNIT } { PUSH string \"failed assertion\" ; FAILWITH } } |} : bool -> unit)] b
[@private] [@hidden] [@inline] let assert_with_error (b : bool) (s : string) = [%Michelson ({| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |} : bool * string -> unit)] (b, s)
[@private] [@hidden] [@inline] let assert_some (type a) (v : a option) : unit = [%Michelson ({| { IF_NONE { PUSH string \"failed assert some\" ; FAILWITH } { DROP ; UNIT } } |} : a option -> unit)] v
[@private] [@hidden] [@inline] let assert_some_with_error (type a) (v : a option) (s : string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |} : a option * string -> unit)] (v, s)
[@private] [@hidden] [@inline] let assert_none (type a) (v : a option) : unit = [%Michelson ({| { IF_NONE { UNIT } { PUSH string \"failed assert none\" ; FAILWITH } } |} : a option -> unit)] v
[@private] [@hidden] [@inline] let assert_none_with_error (type a) (v : a option) (s : string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |} : a option * string -> unit)] (v, s)
[@private] [@hidden] [@inline] let abs (i : int) : nat = [%Michelson ({| { ABS } |} : int -> nat)] i
[@private] [@hidden] [@inline] let is_nat (i : int) : nat option = [%Michelson ({| { ISNAT } |} : int -> nat option)] i
[@private] [@hidden] [@inline] let true : bool = [%external \"TRUE\"]
[@private] [@hidden] [@inline] let false : bool = [%external \"FALSE\"]
[@private] [@hidden] [@inline] let unit : unit = [%external \"UNIT\"]
[@private] [@hidden] [@inline] let failwith (type a) (v : a) : a external_failwith = [%external \"FAILWITH\"] v
[@private] [@hidden] [@inline] let int (type a) (v : a) : a external_int = [%Michelson ({| { INT } |} : a -> a external_int)] v
[@private] [@hidden] [@inline] let ediv (type a b) (l : a) (r : b) : (a, b) external_ediv = [%Michelson ({| { UNPAIR ; EDIV } |} : a * b -> (a, b) external_ediv)] (l, r)
"

let lib_proto_jakarta (s : Syntax_types.t) =
  match s with
  | PascaLIGO _ | ReasonLIGO | JsLIGO ->"
module Tezos = struct
  [@hidden] [@inline] let get_contract (type a) (a : address) : (a contract) = [%external \"CONTRACT\"] a
  [@hidden] [@inline] let get_contract_opt (type a) (a : address) : (a contract) option = [%external \"CONTRACT_OPT\"] a
  [@hidden] [@inline] let get_contract_with_error (type a) ((a, s) : address * string) : a contract = [%external \"CONTRACT_WITH_ERROR\"] a s
  (* [@hidden] [@inline] let get_entrypoint_opt (type a) ((s, a) : string * address) : (a contract) option = [%external \"CONTRACT_ENTRYPOINT_OPT\"] s a *)
  [@thunk] [@hidden] [@inline] let balance : tez = [%Michelson ({| { DROP ; BALANCE } |} : unit -> tez)] ()
  [@thunk] [@hidden] [@inline] let amount : tez = [%Michelson ({| { DROP ; AMOUNT } |} : unit -> tez)] ()
  [@thunk] [@hidden] [@inline] let now : timestamp = [%Michelson ({| { DROP ; NOW } |} : unit -> timestamp)] ()
  [@thunk] [@hidden] [@inline] let sender : address = [%Michelson ({| { DROP ; SENDER } |} : unit -> address)] ()
  [@thunk] [@hidden] [@inline] let source : address = [%Michelson ({| { DROP ; SOURCE } |} : unit -> address)] ()
  [@thunk] [@hidden] [@inline] let level : nat = [%Michelson ({| { DROP ; LEVEL } |} : unit -> nat)] ()
  [@thunk] [@hidden] [@inline] let self_address : address = [%external \"SELF_ADDRESS\"]
  [@thunk] [@hidden] [@inline] let chain_id : chain_id = [%Michelson ({| { DROP ; CHAIN_ID } |} : unit -> chain_id)] ()
  [@thunk] [@hidden] [@inline] let total_voting_power : nat = [%Michelson ({| { DROP ; TOTAL_VOTING_POWER } |} : unit -> nat)] ()
  [@hidden] [@inline] let min_block_time : unit -> nat = [%Michelson ({| { DROP; MIN_BLOCK_TIME } |} : unit -> nat) ]
  (* [@thunk] [@hidden] [@inline] let self (type a) (s : string) : a contract = [%external \"SELF\"] s *)
  [@hidden] [@inline] let voting_power (kh : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat)] kh
  [@hidden] [@inline] let address (type a) (c : a contract) : address = [%external \"ADDRESS\"] c
  [@hidden] [@inline] let implicit_account (kh : key_hash) : unit contract = [%external \"IMPLICIT_ACCOUNT\"] kh
  [@hidden] [@inline] let create_ticket (type a) ((v, n) : a * nat) : a ticket = [%Michelson ({| { UNPAIR ; TICKET } |} : a * nat -> a ticket)] (v, n)
  [@hidden] [@inline] let join_tickets (type a) (t : a ticket * a ticket) : (a ticket) option = [%Michelson ({| { JOIN_TICKETS } |} : a ticket * a ticket -> a ticket option)] t
  [@hidden] [@inline] let split_ticket (type a) ((t, p) : (a ticket) * (nat * nat)) : (a ticket * a ticket) option = [%Michelson ({| { UNPAIR ; SPLIT_TICKET } |} : a ticket * (nat * nat) -> (a ticket * a ticket) option)] (t, p)
  [@hidden] [@inline] let read_ticket (type a) (t : a ticket) : (address * (a * nat)) * a ticket = [%Michelson ({| { READ_TICKET ; PAIR } |} : a ticket -> (address * (a * nat)) * a ticket)] t
  (* [@hidden] [@inline] let create_contract (type a b) ((c, kh, mu, s) : (a * b -> operation list * b) * key_hash * tez * b) : operation * address = [%external \"CREATE_CONTRACT\"] c kh mu s *)
  [@hidden] [@inline] let never (type a) (n : never) : a = [%Michelson ({| { NEVER } |} : never -> a)] n
  [@hidden] [@inline] let pairing_check (l : (bls12_381_g1 * bls12_381_g2) list) : bool = [%Michelson ({| { PAIRING_CHECK } |} : (bls12_381_g1 * bls12_381_g2) list -> bool)] l
  [@hidden] [@inline] let open_chest ((ck, c, n) : chest_key * chest * nat) : chest_opening_result = [%external \"OPEN_CHEST\"] ck c n
  [@hidden] [@inline] let call_view (type a b) ((s, x, a) : string * a * address)  : b option = [%external \"VIEW\"] s x a
  [@hidden] [@inline] let constant (type a) (s : string) : a = [%external \"GLOBAL_CONSTANT\"] s
  [@hidden] [@inline] let transaction (type a) ((a, mu, c) : a * tez * a contract) : operation = [%external \"CALL\"] a mu c
  [@hidden] [@inline] let set_delegate (o : key_hash option) : operation = [%external \"SET_DELEGATE\"] o
end
module Bitwise = struct
  (* [@hidden] [@inline] let and (type a b) ((l, r) : (a, b)) : (a, b) external_and = [%external \"AND\"] l r *)
  [@hidden] [@inline] let xor ((l, r) : nat * nat) : nat = [%external \"XOR\"] l r
  (* [@hidden] [@inline] let or ((l, r) : nat * nat) : nat = [%external \"OR\"] l r *)
  [@hidden] [@inline] let shift_left ((l, r) : nat * nat) : nat = [%external \"LSL\"] l r
  [@hidden] [@inline] let shift_right ((l, r) : nat * nat) : nat = [%external \"LSR\"] l r
end
module Big_map = struct
  [@hidden] [@inline] let empty (type k v) : (k, v) big_map = [%external \"BIG_MAP_EMPTY\"]
  [@hidden] [@inline] let mem (type k v) ((k, m) : k * (k, v) big_map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) big_map -> bool)] (k, m)
  [@hidden] [@inline] let add (type k v) ((k, v, m) : k * v * (k, v) big_map) : (k, v) big_map = [%external \"MAP_ADD\"] k v m
  [@hidden] [@inline] let remove (type k v) ((k, m) : k * (k, v) big_map) : (k, v) big_map = [%external \"MAP_REMOVE\"] k m
  [@hidden] [@inline] let update (type k v) ((k, v, m) : k * v option * (k, v) big_map) : (k, v) big_map = [%external \"MAP_UPDATE\"] k v m
  [@hidden] [@inline] let get_and_update (type k v) ((k, v, m) : k * v option * (k, v) big_map) : v option * (k, v) big_map = [%external \"BIG_MAP_GET_AND_UPDATE\"] k v m
  [@hidden] [@inline] let find_opt (type k v) ((k, m) : k * (k, v) big_map) : v option = [%external \"MAP_FIND_OPT\"] k m
  [@hidden] [@inline] let find (type k v) ((k, m) : k * (k, v) big_map) : v = [%external \"MAP_FIND\"] k m
end
module Map = struct
  [@hidden] [@inline] let empty (type k v) : (k, v) map = [%external \"MAP_EMPTY\"]
  [@hidden] [@inline] let size (type k v) (m : (k, v) map) : nat = [%Michelson ({| { SIZE } |} : (k, v) map -> nat)] m
  [@hidden] [@inline] let mem (type k v) ((k, m) : k * (k, v) map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) map -> bool)] (k, m)
  [@hidden] [@inline] let add (type k v) ((k, v, m) : k * v * (k, v) map) : (k, v) map = [%external \"MAP_ADD\"] k v m
  [@hidden] [@inline] let remove (type k v) ((k, m) : k * (k, v) map) : (k, v) map = [%external \"MAP_REMOVE\"] k m
  [@hidden] [@inline] let update (type k v) ((k, v, m) : k * v option * (k, v) map) : (k, v) map = [%external \"MAP_UPDATE\"] k v m
  [@hidden] [@inline] let get_and_update (type k v) ((k, v, m) : k * v option * (k, v) map) : v option * (k, v) map = [%external \"MAP_GET_AND_UPDATE\"] k v m
  [@hidden] [@inline] let find_opt (type k v) ((k, m) : k * (k, v) map) : v option = [%external \"MAP_FIND_OPT\"] k m
  [@hidden] [@inline] let find (type k v) ((k, m) : k * (k, v) map) : v = [%external \"MAP_FIND\"] k m
  [@hidden] [@inline] let iter (type k v) ((f, m) : (k * v -> unit) * (k, v) map) : unit = [%external \"MAP_ITER\"] f m
  [@hidden] [@inline] let map (type k v w) ((f, m) : (k * v -> w) * (k, v) map) : (k, w) map = [%external \"MAP_MAP\"] f m
  [@hidden] [@inline] let fold (type k v c) ((f, m, i) : (c * (k * v) -> c) * (k, v) map * c) : c = [%external \"MAP_FOLD\"] f m i
end
module Set = struct
  [@hidden] [@inline] let empty (type a) : a set = [%external \"SET_EMPTY\"]
  [@hidden] [@inline] let mem (type a) ((x, s) : a * a set) : bool = [%external \"SET_MEM\"] x s
  [@hidden] [@inline] let add (type a) ((x, s) : a * a set) : a set = [%external \"SET_ADD\"] x s
  [@hidden] [@inline] let remove (type a) ((x, s) : a * a set) : a set = [%external \"SET_REMOVE\"] x s
  [@hidden] [@inline] let update (type a) ((x, b, s) : a * bool * a set) = [%external \"SET_UPDATE\"] x b s
  [@hidden] [@inline] let iter (type a) ((f, s) : (a -> unit) * a set) : unit = [%external \"SET_ITER\"] f s
  [@hidden] [@inline] let fold (type a b) ((f, s, i) : (b * a -> b) * a set * b) : b = [%external \"SET_FOLD\"] f s i
  [@hidden] [@inline] let fold_desc (type a b) ((f, s, i) : (a * b -> b) * a set * b) : b = [%external \"SET_FOLD_DESC\"] f s i
  [@hidden] [@inline] let size (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)]  s
  [@hidden] [@inline] let cardinal (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)]  s
end
module List = struct
  [@hidden] [@inline] let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  [@hidden] [@inline] let size (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  [@hidden] [@inline] let head_opt (type a) (xs : a list) : a option = [%external \"LIST_HEAD_OPT\"] xs
  [@hidden] [@inline] let tail_opt (type a) (xs : a list) : (a list) option = [%external \"LIST_TAIL_OPT\"] xs
  [@hidden] [@inline] let map (type a b) ((f, xs) : (a -> b) * a list) : b list = [%external \"LIST_MAP\"] f xs
  [@hidden] [@inline] let iter (type a) ((f, xs) : (a -> unit) * a list): unit = [%external \"LIST_ITER\"] f xs
  [@hidden] [@inline] let fold (type a b) ((f, xs, i) : (b * a -> b) * a list * b) : b = [%external \"LIST_FOLD\"] f xs i
  [@hidden] [@inline] let fold_left (type a b) ((f, i, xs) : (b * a -> b) * b * a list) : b = [%external \"LIST_FOLD_LEFT\"] f i xs
  [@hidden] [@inline] let fold_right (type a b) ((f, xs, i) : (a * b -> b) * a list * b) : b = [%external \"LIST_FOLD_RIGHT\"] f xs i
  [@hidden] [@inline] let cons (type a) ((x, xs) : a * a list) : a list = [%external \"CONS\"] x xs
end
module String = struct
  [@hidden] [@inline] let concat ((b1, b2) : string * string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] (b1, b2)
  [@hidden] [@inline] let sub ((s, l, b) : nat * nat * string) : string = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] (s, l, b)
  [@hidden] [@inline] let length (b : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] b
end
module Option = struct
  [@hidden] [@inline] let unopt (type a) (v : a option) : a = [%external \"UNOPT\"] v
  [@hidden] [@inline] let unopt_with_error (type a) ((v, s) : (a option) * string) : a = [%external \"UNOPT_WITH_ERROR\"] v s
  (* [@hidden] [@inline] let map (type a b) ((f, v) : (a -> b) * (a option)) : b option = [%external \"OPTION_MAP\"] f v *)
end
module Bytes = struct
  [@hidden] [@inline] let concat ((b1, b2) : bytes * bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] (b1, b2)
  [@hidden] [@inline] let sub ((s, l, b) : nat * nat * bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (s, l, b)
  [@hidden] [@inline] let pack (type a) (v : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] v
  [@hidden] [@inline] let unpack (type a) (b : bytes) : a option = [%external \"BYTES_UNPACK\"] b
  [@hidden] [@inline] let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b
end
module Crypto = struct
  [@hidden] [@inline] let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
  [@hidden] [@inline] let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k
  [@hidden] [@inline] let check ((k, s, b) : key * signature * bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end
[@private] [@hidden] [@inline] let assert (b : bool) : unit = [%Michelson ({| { IF { UNIT } { PUSH string \"failed assertion\" ; FAILWITH } } |} : bool -> unit)] b
[@private] [@hidden] [@inline] let assert_with_error ((b, s) : bool * string) = [%Michelson ({| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |} : bool * string -> unit)] (b, s)
[@private] [@hidden] [@inline] let assert_some (type a) (v : a option) : unit = [%Michelson ({| { IF_NONE { PUSH string \"failed assert some\" ; FAILWITH } { DROP ; UNIT } } |} : a option -> unit)] v
[@private] [@hidden] [@inline] let assert_some_with_error (type a) ((v, s) : a option * string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |} : a option * string -> unit)] (v, s)
[@private] [@hidden] [@inline] let assert_none (type a) (v : a option) : unit = [%Michelson ({| { IF_NONE { UNIT } { PUSH string \"failed assert none\" ; FAILWITH } } |} : a option -> unit)] v
[@private] [@hidden] [@inline] let assert_none_with_error (type a) ((v, s) : a option * string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |} : a option * string -> unit)] (v, s)
[@private] [@hidden] [@inline] let abs (i : int) : nat = [%Michelson ({| { ABS } |} : int -> nat)] i
[@private] [@hidden] [@inline] let is_nat (i : int) : nat option = [%Michelson ({| { ISNAT } |} : int -> nat option)] i
[@private] [@hidden] [@inline] let true : bool = [%external \"TRUE\"]
[@private] [@hidden] [@inline] let false : bool = [%external \"FALSE\"]
[@private] [@hidden] [@inline] let unit : unit = [%external \"UNIT\"]
[@private] [@hidden] [@inline] let failwith (type a) (v : a) : a external_failwith = [%external \"FAILWITH\"] v
[@private] [@hidden] [@inline] let int (type a) (v : a) : a external_int = [%Michelson ({| { INT } |} : a -> a external_int)] v
[@private] [@hidden] [@inline] let ediv (type a b) ((l, r) : (a * b)) : (a, b) external_u_ediv = [%Michelson ({| { UNPAIR ; EDIV } |} : a * b -> (a, b) external_u_ediv)] (l, r)
"
  | CameLIGO -> "
module Tezos = struct
  [@hidden] [@inline] let get_contract (type a) (a : address) : (a contract) = [%external \"CONTRACT\"] a
  [@hidden] [@inline] let get_contract_opt (type a) (a : address) : (a contract) option = [%external \"CONTRACT_OPT\"] a
  [@hidden] [@inline] let get_contract_with_error (type a) (a : address) (s : string) : a contract = [%external \"CONTRACT_WITH_ERROR\"] a s
  (* [@hidden] [@inline] let get_entrypoint_opt (type a) (s : string) (a : address) : (a contract) option = [%external \"CONTRACT_ENTRYPOINT_OPT\"] s a *)
  [@thunk] [@hidden] [@inline] let balance : tez = [%Michelson ({| { DROP ; BALANCE } |} : unit -> tez)] ()
  [@thunk] [@hidden] [@inline] let amount : tez = [%Michelson ({| { DROP ; AMOUNT } |} : unit -> tez)] ()
  [@thunk] [@hidden] [@inline] let now : timestamp = [%Michelson ({| { DROP ; NOW } |} : unit -> timestamp)] ()
  [@thunk] [@hidden] [@inline] let sender : address = [%Michelson ({| { DROP ; SENDER } |} : unit -> address)] ()
  [@thunk] [@hidden] [@inline] let source : address = [%Michelson ({| { DROP ; SOURCE } |} : unit -> address)] ()
  [@thunk] [@hidden] [@inline] let level : nat = [%Michelson ({| { DROP ; LEVEL } |} : unit -> nat)] ()
  [@thunk] [@hidden] [@inline] let self_address : address = [%external \"SELF_ADDRESS\"]
  [@thunk] [@hidden] [@inline] let chain_id : chain_id = [%Michelson ({| { DROP ; CHAIN_ID } |} : unit -> chain_id)] ()
  [@thunk] [@hidden] [@inline] let total_voting_power : nat = [%Michelson ({| { DROP ; TOTAL_VOTING_POWER } |} : unit -> nat)] ()
  [@hidden] [@inline] let min_block_time : unit -> nat = [%Michelson ({| { DROP; MIN_BLOCK_TIME } |} : unit -> nat) ]
  (* [@thunk] [@hidden] [@inline] let self (type a) (s : string) : a contract = [%external \"SELF\"] s *)
  [@hidden] [@inline] let voting_power (kh : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat)] kh
  [@hidden] [@inline] let address (type a) (c : a contract) : address = [%external \"ADDRESS\"] c
  [@hidden] [@inline] let implicit_account (kh : key_hash) : unit contract = [%external \"IMPLICIT_ACCOUNT\"] kh
  [@hidden] [@inline] let create_ticket (type a) (v : a) (n : nat) : a ticket = [%Michelson ({| { UNPAIR ; TICKET } |} : a * nat -> a ticket)] (v, n)
  [@hidden] [@inline] let join_tickets (type a) (t : a ticket * a ticket) : (a ticket) option = [%Michelson ({| { JOIN_TICKETS } |} : a ticket * a ticket -> a ticket option)] t
  [@hidden] [@inline] let split_ticket (type a) (t : a ticket) (p : nat * nat) : (a ticket * a ticket) option = [%Michelson ({| { UNPAIR ; SPLIT_TICKET } |} : a ticket * (nat * nat) -> (a ticket * a ticket) option)] (t, p)
  [@hidden] [@inline] let read_ticket (type a) (t : a ticket) : (address * (a * nat)) * a ticket = [%Michelson ({| { READ_TICKET ; PAIR } |} : a ticket -> (address * (a * nat)) * a ticket)] t
  (* [@hidden] [@inline] let create_contract (type a b) (c : a * b -> operation list * b) (kh : key_hash) (mu : tez) (s : b) : operation * address = [%external \"CREATE_CONTRACT\"] c kh mu s *)
  [@hidden] [@inline] let never (type a) (n : never) : a = [%Michelson ({| { NEVER } |} : never -> a)] n
  [@hidden] [@inline] let pairing_check (l : (bls12_381_g1 * bls12_381_g2) list) : bool = [%Michelson ({| { PAIRING_CHECK } |} : (bls12_381_g1 * bls12_381_g2) list -> bool)] l
  [@hidden] [@inline] let open_chest (ck : chest_key) (c : chest) (n : nat) : chest_opening_result = [%external \"OPEN_CHEST\"] ck c n
  [@hidden] [@inline] let call_view (type a b) (s : string) (x : a) (a : address)  : b option = [%external \"VIEW\"] s x a
  [@hidden] [@inline] let constant (type a) (s : string) : a = [%external \"GLOBAL_CONSTANT\"] s
  [@hidden] [@inline] let transaction (type a) (a : a) (mu : tez) (c : a contract) : operation = [%external \"CALL\"] a mu c
  [@hidden] [@inline] let set_delegate (o : key_hash option) : operation = [%external \"SET_DELEGATE\"] o
end
module Bitwise = struct
  (* [@hidden] [@inline] let and (type a b) (l : a) (r : b) : (a, b) external_and = [%external \"AND\"] l r *)
  [@hidden] [@inline] let xor (l : nat) (r : nat) : nat = [%external \"XOR\"] l r
  (* [@hidden] [@inline] let or (l : nat) (r : nat) : nat = [%external \"OR\"] l r *)
  [@hidden] [@inline] let shift_left (l : nat) (r : nat) : nat = [%external \"LSL\"] l r
  [@hidden] [@inline] let shift_right (l : nat) (r : nat) : nat = [%external \"LSR\"] l r
end
module Big_map = struct
  [@hidden] [@inline] let empty (type k v) : (k, v) big_map = [%external \"BIG_MAP_EMPTY\"]
  [@hidden] [@inline] let mem (type k v) (k : k) (m : (k, v) big_map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) big_map -> bool)] (k, m)
  [@hidden] [@inline] let add (type k v) (k : k) (v : v) (m : (k, v) big_map) : (k, v) big_map = [%external \"MAP_ADD\"] k v m
  [@hidden] [@inline] let remove (type k v) (k : k) (m : (k, v) big_map) : (k, v) big_map = [%external \"MAP_REMOVE\"] k m
  [@hidden] [@inline] let update (type k v) (k : k) (v : v option) (m : (k, v) big_map) : (k, v) big_map = [%external \"MAP_UPDATE\"] k v m
  [@hidden] [@inline] let get_and_update (type k v) (k : k) (v : v option) (m : (k, v) big_map) : v option * (k, v) big_map = [%external \"BIG_MAP_GET_AND_UPDATE\"] k v m
  [@hidden] [@inline] let find_opt (type k v) (k : k) (m : (k, v) big_map) : v option = [%external \"MAP_FIND_OPT\"] k m
  [@hidden] [@inline] let find (type k v) (k : k) (m : (k, v) big_map) : v = [%external \"MAP_FIND\"] k m
end
module Map = struct
  [@hidden] [@inline] let empty (type k v) : (k, v) map = [%external \"MAP_EMPTY\"]
  [@hidden] [@inline] let size (type k v) (m : (k, v) map) : nat = [%Michelson ({| { SIZE } |} : (k, v) map -> nat)] m
  [@hidden] [@inline] let mem (type k v) (k : k) (m : (k, v) map) : bool = [%Michelson ({| { UNPAIR ; MEM } |} : k * (k, v) map -> bool)] (k, m)
  [@hidden] [@inline] let add (type k v) (k : k) (v : v) (m : (k, v) map) : (k, v) map = [%external \"MAP_ADD\"] k v m
  [@hidden] [@inline] let remove (type k v) (k : k) (m : (k, v) map) : (k, v) map = [%external \"MAP_REMOVE\"] k m
  [@hidden] [@inline] let update (type k v) (k : k) (v : v option) (m : (k, v) map) : (k, v) map = [%external \"MAP_UPDATE\"] k v m
  [@hidden] [@inline] let get_and_update (type k v) (k : k) (v : v option) (m : (k, v) map) : v option * (k, v) map = [%external \"MAP_GET_AND_UPDATE\"] k v m
  [@hidden] [@inline] let find (type k v) (k : k) (m : (k, v) map) : v = [%external \"MAP_FIND\"] k m
  [@hidden] [@inline] let find_opt (type k v) (k : k) (m : (k, v) map) : v option = [%external \"MAP_FIND_OPT\"] k m
  [@hidden] [@inline] let iter (type k v) (f : k * v -> unit) (m : (k, v) map) : unit = [%external \"MAP_ITER\"] f m
  [@hidden] [@inline] let map (type k v w) (f : k * v -> w) (m : (k, v) map) : (k, w) map = [%external \"MAP_MAP\"] f m
  [@hidden] [@inline] let fold (type k v c) (f : c * (k * v) -> c) (m : (k, v) map) (i : c) : c = [%external \"MAP_FOLD\"] f m i
end
module Set = struct
  [@hidden] [@inline] let empty (type a) : a set = [%external \"SET_EMPTY\"]
  [@hidden] [@inline] let mem (type a) (x : a) (s : a set) : bool = [%external \"SET_MEM\"] x s
  [@hidden] [@inline] let add (type a) (x : a) (s : a set) : a set = [%external \"SET_ADD\"] x s
  [@hidden] [@inline] let remove (type a) (x : a) (s : a set) : a set = [%external \"SET_REMOVE\"] x s
  [@hidden] [@inline] let update (type a) (x : a) (b : bool) (s : a set) = [%external \"SET_UPDATE\"] x b s
  [@hidden] [@inline] let iter (type a) (f : a -> unit) (s : a set) : unit = [%external \"SET_ITER\"] f s
  [@hidden] [@inline] let fold (type a b) (f : b * a -> b) (s : a set) (i : b) : b = [%external \"SET_FOLD\"] f s i
  [@hidden] [@inline] let fold_desc (type a b) (f : a * b -> b) (s : a set) (i : b) : b = [%external \"SET_FOLD_DESC\"] f s i
  [@hidden] [@inline] let size (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)]  s
  [@hidden] [@inline] let cardinal (type a) (s : a set) : nat = [%Michelson ({| { SIZE } |} : a set -> nat)] s
end
module List = struct
  [@hidden] [@inline] let length (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  [@hidden] [@inline] let size (type a) (xs : a list) : nat = [%Michelson ({| { SIZE } |} : a list -> nat)]  xs
  [@hidden] [@inline] let head_opt (type a) (xs : a list) : a option = [%external \"LIST_HEAD_OPT\"] xs
  [@hidden] [@inline] let tail_opt (type a) (xs : a list) : (a list) option = [%external \"LIST_TAIL_OPT\"] xs
  [@hidden] [@inline] let map (type a b) (f : a -> b) (xs : a list) : b list = [%external \"LIST_MAP\"] f xs
  [@hidden] [@inline] let iter (type a) (f : a -> unit) (xs : a list): unit = [%external \"LIST_ITER\"] f xs
  [@hidden] [@inline] let fold (type a b) (f : b * a -> b) (xs : a list) (i : b) : b = [%external \"LIST_FOLD\"] f xs i
  [@hidden] [@inline] let fold_left (type a b) (f : b * a -> b) (i : b) (xs : a list) : b = [%external \"LIST_FOLD_LEFT\"] f i xs
  [@hidden] [@inline] let fold_right (type a b) (f : a * b -> b) (xs : a list) (i : b) : b = [%external \"LIST_FOLD_RIGHT\"] f xs i
  [@hidden] [@inline] let cons (type a) (x : a) (xs : a list) : a list = [%external \"CONS\"] x xs
end
module String = struct
  [@hidden] [@inline] let concat (b1 : string) (b2 : string) : string = [%Michelson ({| { UNPAIR ; CONCAT } |} : string * string -> string)] (b1, b2)
  [@hidden] [@inline] let sub (s : nat) (l : nat) (b : string) : string = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * string -> string)] (s, l, b)
  [@hidden] [@inline] let length (b : string) : nat = [%Michelson ({| { SIZE } |} : string -> nat)] b
end
module Option = struct
  [@hidden] [@inline] let unopt (type a) (v : a option) : a = [%external \"UNOPT\"] v
  [@hidden] [@inline] let unopt_with_error (type a) (v : a option) (s : string) : a = [%external \"UNOPT_WITH_ERROR\"] v s
  (* [@hidden] [@inline] let map (type a b) (f : a -> b) (v : a option) : b option = [%external \"OPTION_MAP\"] f v *)
end
module Bytes = struct
  [@hidden] [@inline] let concat (b1 : bytes) (b2 : bytes) : bytes = [%Michelson ({| { UNPAIR ; CONCAT } |} : bytes * bytes -> bytes)] (b1, b2)
  [@hidden] [@inline] let sub (s : nat) (l : nat) (b : bytes) : bytes = [%Michelson ({| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } |} : nat * nat * bytes -> bytes)] (s, l, b)
  [@hidden] [@inline] let pack (type a) (v : a) : bytes = [%Michelson ({| { PACK } |} : a -> bytes)] v
  [@hidden] [@inline] let unpack (type a) (b : bytes) : a option = [%external \"BYTES_UNPACK\"] b
  [@hidden] [@inline] let length (b : bytes) : nat = [%Michelson ({| { SIZE } |} : bytes -> nat)] b
end
module Crypto = struct
  [@hidden] [@inline] let blake2b (b : bytes) : bytes = [%Michelson ({| { BLAKE2B } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha256 (b : bytes) : bytes = [%Michelson ({| { SHA256 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha512 (b : bytes) : bytes = [%Michelson ({| { SHA512 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let sha3 (b : bytes) : bytes = [%Michelson ({| { SHA3 } |} : bytes -> bytes)] b
  [@hidden] [@inline] let keccak (b : bytes) : bytes = [%Michelson ({| { KECCAK } |} : bytes -> bytes)] b
  [@hidden] [@inline] let hash_key (k : key) : key_hash = [%Michelson ({| { HASH_KEY } |} : key -> key_hash)] k
  [@hidden] [@inline] let check (k : key) (s : signature) (b : bytes) : bool = [%Michelson ({| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |} : key * signature * bytes -> bool)] (k, s, b)
end
[@private] [@hidden] [@inline] let assert (b : bool) : unit = [%Michelson ({| { IF { UNIT } { PUSH string \"failed assertion\" ; FAILWITH } } |} : bool -> unit)] b
[@private] [@hidden] [@inline] let assert_with_error (b : bool) (s : string) = [%Michelson ({| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |} : bool * string -> unit)] (b, s)
[@private] [@hidden] [@inline] let assert_some (type a) (v : a option) : unit = [%Michelson ({| { IF_NONE { PUSH string \"failed assert some\" ; FAILWITH } { DROP ; UNIT } } |} : a option -> unit)] v
[@private] [@hidden] [@inline] let assert_some_with_error (type a) (v : a option) (s : string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |} : a option * string -> unit)] (v, s)
[@private] [@hidden] [@inline] let assert_none (type a) (v : a option) : unit = [%Michelson ({| { IF_NONE { UNIT } { PUSH string \"failed assert none\" ; FAILWITH } } |} : a option -> unit)] v
[@private] [@hidden] [@inline] let assert_none_with_error (type a) (v : a option) (s : string) : unit = [%Michelson ({| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |} : a option * string -> unit)] (v, s)
[@private] [@hidden] [@inline] let abs (i : int) : nat = [%Michelson ({| { ABS } |} : int -> nat)] i
[@private] [@hidden] [@inline] let is_nat (i : int) : nat option = [%Michelson ({| { ISNAT } |} : int -> nat option)] i
[@private] [@hidden] [@inline] let true : bool = [%external \"TRUE\"]
[@private] [@hidden] [@inline] let false : bool = [%external \"FALSE\"]
[@private] [@hidden] [@inline] let unit : unit = [%external \"UNIT\"]
[@private] [@hidden] [@inline] let failwith (type a) (v : a) : a external_failwith = [%external \"FAILWITH\"] v
[@private] [@hidden] [@inline] let int (type a) (v : a) : a external_int = [%Michelson ({| { INT } |} : a -> a external_int)] v
[@private] [@hidden] [@inline] let ediv (type a b) (l : a) (r : b) : (a, b) external_ediv = [%Michelson ({| { UNPAIR ; EDIV } |} : a * b -> (a, b) external_ediv)] (l, r)
"

let stdlib ~options syntax =
  let lib =
    let open Compiler_options in 
    match options.middle_end.protocol_version with
    | Environment.Protocols.Jakarta -> lib_proto_jakarta syntax
    | Environment.Protocols.Ithaca -> lib syntax
  in
  match Simple_utils.Trace.to_stdlib_result @@
          Ligo_compile.Utils.type_contract_string ~add_warning:(fun _ -> ()) ~options CameLIGO lib with
  | Ok s -> s
  | Error e ->
     let error_msg = Format.asprintf "%a" (Main_errors.Formatter.error_ppformat ~display_format:Human_readable) e in
     failwith ("Error compiling the stdlib: " ^ error_msg)

module LanguageMap = Simple_utils.Map.Make(struct type t = Syntax_types.t let compare = Syntax_types.compare end)
let cached = ref LanguageMap.empty

let typed ~options syntax =
  Helpers.internalize_typed @@
    match LanguageMap.find_opt syntax @@ ! cached with
    | None ->
       let typed, core = stdlib ~options syntax in
       cached := LanguageMap.add syntax (typed, core) @@ ! cached;
       typed
    | Some (typed, _) -> typed

let core ~options syntax =
  Helpers.internalize_core @@
    match LanguageMap.find_opt syntax @@ ! cached with
    | None ->
       let typed, core = stdlib ~options syntax in
       cached := LanguageMap.add syntax (typed, core) @@ ! cached;
       core
    | Some (_, core) -> core
