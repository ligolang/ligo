(* basic types *)
type string = "%constant:string"
type bytes = "%constant:bytes"
type int = "%constant:int"
type nat = "%constant:nat"
type unit = "%constant:unit"

(* michelson base *)
type operation = "%constant:operation"
type tez = "%constant:tez"
type address = "%constant:address"
type signature = "%constant:signature"
type key = "%constant:key"
type key_hash = "%constant:key_hash"
type timestamp = "%constant:timestamp"
type list = "%constant:list"
type big_map = "%constant:big_map"
type map = "%constant:map"
type set = "%constant:set"
type contract = "%constant:contract"
type michelson_or = "%constant:michelson_or"
type michelson_pair = "%constant:michelson_pair"
type chain_id = "%constant:chain_id"
type baker_hash = "%constant:baker_hash"
type pvss_key = "%constant:pvss_key"
type sapling_state = "%constant:sapling_state"
type sapling_transaction = "%constant:sapling_transaction"
type baker_operation = "%constant:baker_operation"
type bls12_381_g1 = "%constant:bls12_381_g1"
type bls12_381_g2 = "%constant:bls12_381_g2"
type bls12_381_fr = "%constant:bls12_381_fr"
type never = "%constant:never"
type ticket = "%constant:ticket"
type chest = "%constant:chest"
type chest_key = "%constant:chest_key"

(* external "custom" typers *)
type external_bytes = "%constant:external_bytes"
type external_int = "%constant:external_int"
type external_ediv = "%constant:external_ediv"
type external_and = "%constant:external_and"
type external_or = "%constant:external_or"
type external_xor = "%constant:external_xor"
type external_lsl = "%constant:external_lsl"
type external_lsr = "%constant:external_lsr"
type external_map_find_opt = "%constant:external_map_find_opt"
type external_map_add = "%constant:external_map_add"
type external_map_remove = "%constant:external_map_remove"
type external_map_remove_value = "%constant:external_map_remove_value"
(* type dynamic_entrypoint represent a typed key in dynamic_entrypoints *)
type dynamic_entrypoint = "%constant:dynamic_entrypoint"

type ('p, 's) entrypoint = 'p -> 's -> operation list * 's

let failwith (type a b) (x : a) = [%michelson ({| { FAILWITH } |} x : b)]

type bool = False | True
type 'a option = None | Some of 'a


module Tezos = struct

  let get_balance (_u : unit) : tez = [%michelson ({| { BALANCE } |} : tez)]
  let get_amount (_u : unit) : tez = [%michelson ({| { AMOUNT } |} : tez)]
  let get_now (_u : unit) : timestamp = [%michelson ({| { NOW } |} : timestamp)]
  let get_sender (_u : unit) : address = [%michelson ({| { SENDER } |} : address)]
  let get_source (_u : unit) : address = [%michelson ({| { SOURCE } |} : address)]
  let get_level (_u : unit) : nat = [%michelson ({| { LEVEL } |} : nat)]
  let get_self_address (_u : unit) : address = [%michelson ({| { SELF_ADDRESS } |} : address)]
  let get_chain_id (_u : unit) : chain_id = [%michelson ({| { CHAIN_ID } |} : chain_id)]
  let get_total_voting_power (_u : unit) : nat = [%michelson ({| { TOTAL_VOTING_POWER } |} : nat)]
  let get_min_block_time (_u : unit) : nat = [%michelson ({| { MIN_BLOCK_TIME } |} : nat)]
  let voting_power (kh : key_hash) : nat = [%michelson ({| { VOTING_POWER } |} kh : nat)]
  let address (type a) (c : a contract) : address = [%michelson ({| { ADDRESS } |} c : address)]
  let implicit_account (kh : key_hash) : unit contract = [%michelson ({| { IMPLICIT_ACCOUNT } |} kh : unit contract)]
  let join_tickets (type a) (t : a ticket * a ticket) : (a ticket) option = [%michelson ({| { JOIN_TICKETS } |} t : a ticket option)]
  let read_ticket (type a) (t : a ticket) : (address * (a * nat)) * a ticket =
    [%michelson ({| { READ_TICKET ; PAIR } |} t : (address * (a * nat)) * a ticket)]
  let never (type a) (n : never) : a = [%michelson ({| { NEVER } |} n : a)]
  let pairing_check (l : (bls12_381_g1 * bls12_381_g2) list) : bool = [%michelson ({| { PAIRING_CHECK } |} l : bool)]
  let set_delegate (o : key_hash option) : operation = [%michelson ({| { SET_DELEGATE } |} o : operation)]
  [@inline] [@thunk] let self (type a) (s : string) : a contract =
    let _ : a option = [%external ("CHECK_SELF", s)] in
    [%michelson ({| { SELF (annot $0) } |} (s : string) : a contract)]
  [@inline] [@thunk] let constant (type a) (s : string) : a = [%external ("GLOBAL_CONSTANT", s)]
  [@inline] [@thunk] let sapling_empty_state (type sap_a) : sap_a sapling_state =
    [%michelson ({| { SAPLING_EMPTY_STATE (typeopt $0) } |} (None : sap_a option) : sap_a sapling_state)]
  [@inline] [@thunk] let get_contract_opt (type p) (a : address) : (p contract) option =
    [%michelson ({| { CONTRACT (typeopt $0) } |} (None : p option) a : (p contract) option)]
  [@inline] [@thunk] let get_contract (type a) (a : address) : (a contract) =
    let v = get_contract_opt a in
    match v with | None -> failwith "bad address for get_contract" | Some c -> c

  let get_contract_with_error (type a) (a : address) (s : string) : a contract =
    let v = get_contract_opt a in
    match v with | None -> failwith s | Some c -> c
  let create_ticket (type a) (v : a) (n : nat) : (a ticket) option = [%michelson ({| { TICKET } |} v n : (a ticket) option)]
#if LEGACY_LAYOUT_TREE
  let transaction (type a) (a : a) (mu : tez) (c : a contract) : operation =
    [%michelson ({| { TRANSFER_TOKENS } |} a mu c : operation)]
#else
  let transaction (type a) (a : a) (mu : tez) (c : a contract) : operation =
    [%michelson ({| { TRANSFER_TOKENS } |} a mu c : operation)]
#endif
  [@inline] [@thunk] let call_view (type a b) (s : string) (x : a) (a : address)  : b option =
    let _ : unit = [%external ("CHECK_CALL_VIEW_LITSTR", s)] in
    [%michelson ({| { VIEW (litstr $0) (typeopt $1) } |} (s : string) (None : b option) x a : b option)]
  let split_ticket (type a) (t : a ticket) (p : nat * nat) : (a ticket * a ticket) option =
    [%michelson ({| { SPLIT_TICKET } |} t p : (a ticket * a ticket) option)]
  [@inline] [@thunk] let create_contract (type p s) (f : p -> s -> operation list * s) (kh : key_hash option) (t : tez) (s : s) : (operation * address) =
      let uncurry (type a b c) (f : a -> b -> c) (xy : a * b) : c = f xy.0 xy.1 in
      [%external ("CREATE_CONTRACT", uncurry f, kh, t, s)]
  [@inline] [@thunk] let get_entrypoint_opt (type p) (e : string) (a : address) : p contract option =
    let _ : unit = [%external ("CHECK_ENTRYPOINT", e)] in
    [%michelson ({| { CONTRACT (annot $0) (typeopt $1) } |} (e : string) (None : p option) a : (p contract) option)]
  [@inline] [@thunk] let get_entrypoint (type p) (e : string) (a : address) : p contract =
    let v = get_entrypoint_opt e a in
    match v with | None -> failwith "bad address for get_entrypoint" | Some c -> c
  [@inline] [@thunk] let emit (type a) (s : string) (v : a) : operation =
    let _ : unit = [%external ("CHECK_EMIT_EVENT", s, v)] in
    [%michelson ({| { EMIT (annot $0) (typeopt $1) } |} (s : string) (None : a option) v : operation)]
  [@inline] [@thunk] let sapling_verify_update (type sap_a) (t : sap_a sapling_transaction) (s : sap_a sapling_state) : (bytes * (int * sap_a sapling_state)) option = [%michelson ({| { SAPLING_VERIFY_UPDATE } |} t s : (bytes * (int * sap_a sapling_state)) option)]
  let open_chest (ck : chest_key) (c : chest) (n : nat) : bytes option =
    [%michelson ({| { OPEN_CHEST } |} ck c n : bytes option)]

end

module Bitwise = struct
  let @and        (type a b) (l : a) (r : b) : (a, b) external_and = [%michelson ({| { AND } |} l r : (a, b) external_and)]
  let xor         (type a b) (l : a) (r : b) : (a, b) external_or  = [%michelson ({| { XOR } |} l r : (a, b) external_or )]
  let @or         (type a b) (l : a) (r : b) : (a, b) external_xor = [%michelson ({| { OR  } |} l r : (a, b) external_xor)]
  let shift_left  (type a b) (l : a) (r : b) : (a, b) external_lsl = [%michelson ({| { LSL } |} l r : (a, b) external_lsl)]
  let shift_right (type a b) (l : a) (r : b) : (a, b) external_lsr = [%michelson ({| { LSR } |} l r : (a, b) external_lsr)]
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
  let map (type a b) (f : a -> b) (xs : a set) : b set =
    fold (fun (x : b set * a) -> add (f x.1) x.0) xs (empty : b set)
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
  let size (b : string) : nat = [%external ("SIZE", b)]
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
  let pack (type a) (v : a) : bytes = [%michelson ({| { PACK } |} v : bytes)]
  let unpack (type a) (b : bytes) : a option = [%michelson ({| { UNPACK (typeopt $0) } |} (None : a option) b : a option)]
  let length (b : bytes) : nat = [%external ("SIZE", b)]

  let concat (b1 : bytes) (b2 : bytes) : bytes = [%external ("CONCAT", b1, b2)]
  let sub (s : nat) (l : nat) (b : bytes) : bytes = [%external ("SLICE", s, l, b)]
end

module Crypto = struct
  let blake2b (b : bytes) : bytes = [%michelson ({| { BLAKE2B } |} b : bytes)]
  let sha256 (b : bytes) : bytes = [%michelson ({| { SHA256 } |} b : bytes)]
  let sha512 (b : bytes) : bytes = [%michelson ({| { SHA512 } |} b : bytes)]
  let sha3 (b : bytes) : bytes = [%michelson ({| { SHA3 } |} b : bytes)]
  let keccak (b : bytes) : bytes = [%michelson ({| { KECCAK } |} b : bytes)]
  let hash_key (k : key) : key_hash = [%michelson ({| { HASH_KEY } |} k : key_hash)]
  let check (k : key) (s : signature) (b : bytes) : bool = [%michelson ({| { CHECK_SIGNATURE } |} k s b : bool)]
end

let assert (b : bool) : unit = if b then () else failwith "failed assertion"
let assert_some (type a) (v : a option) : unit = match v with | None -> failwith "failed assert some" | Some _ -> ()
let assert_none (type a) (v : a option) : unit = match v with | None -> () | Some _ -> failwith "failed assert none"
let abs (i : int) : nat = [%michelson ({| { ABS } |} i : nat)]
let is_nat (i : int) : nat option = [%michelson ({| { ISNAT } |} i : nat option)]
[@inline] let unit : unit = [%external ("UNIT")]
let int (type a) (v : a) : a external_int = [%michelson ({| { INT } |} v : a external_int)]
let nat (v : bytes) : nat = [%michelson ({| { NAT } |} v : nat)]
let bytes (type a) (v : a) : a external_bytes = [%michelson ({| { BYTES } |} v : a external_bytes)]
let ignore (type a) (_ : a) : unit = ()
let curry (type a b c) (f : a * b -> c) (x : a) (y : b) : c = f (x, y)
let uncurry (type a b c) (f : a -> b -> c) (xy : a * b) : c = f xy.0 xy.1

let assert_with_error (b : bool) (s : string) = if b then () else failwith s
let assert_some_with_error (type a) (v : a option) (s : string) : unit = match v with | None -> failwith s | Some _ -> ()
let assert_none_with_error (type a) (v : a option) (s : string) : unit = match v with | None -> () | Some _ -> failwith s
let ediv (type a b) (l : a) (r : b) : (a, b) external_ediv = [%michelson ({| { EDIV } |} l r : (a, b) external_ediv)]

type dynamic_entrypoints = (nat,bytes) big_map
module Dynamic_entrypoints = struct

  [@private] let cast_dynamic_entrypoint (type p s) (x : (p,s) dynamic_entrypoint) : nat =
    [%external ("CAST_DYNAMIC_ENTRYPOINT", x)]

  let set (type p s) :
        (p, s) dynamic_entrypoint
      -> (p, s) entrypoint option
      -> dynamic_entrypoints
      -> dynamic_entrypoints =
    fun k v_opt dyns ->
      let v_opt = Option.map Bytes.pack v_opt in
      Big_map.update (cast_dynamic_entrypoint k) v_opt dyns

  let get (type p s) :
        (p, s) dynamic_entrypoint
      -> dynamic_entrypoints
      -> (p, s) entrypoint option =
    fun k dyns ->
      let res_opt = Big_map.find_opt (cast_dynamic_entrypoint k) dyns in
      Option.map
        (fun dyn_f ->
          let f_opt : (p -> s -> operation list * s) option = Bytes.unpack dyn_f in
          Option.value_exn () f_opt)
        res_opt

  let set_bytes (type p s) :
        (p, s) dynamic_entrypoint
      -> bytes option
      -> dynamic_entrypoints
      -> dynamic_entrypoints =
    fun k v_opt dyns ->
      Big_map.update (cast_dynamic_entrypoint k) v_opt dyns
end

type michelson_program = "%constant:michelson_program"
type typed_address = "%constant:typed_address"
type mutation = "%constant:mutation"
type michelson_contract = "%constant:michelson_contract"
type pbt_gen = "%constant:pbt_gen"
type int64 = "%constant:int64"
type views = "%constant:views"

type test_exec_error_balance_too_low =
  { contract_balance : tez;
    contract_too_low : address;
    spend_request : tez }

type test_exec_error =
  | Balance_too_low of test_exec_error_balance_too_low
  | Other of string
  | Rejected of michelson_program * address

type test_exec_result =
  | Fail of test_exec_error
  | Success of nat

type test_baker_policy =
  | By_account of address
  | By_round of int
  | Excluding of address list

type 'a pbt_test = ('a pbt_gen) * ('a -> bool)
type 'a pbt_result = Fail of 'a | Success

type 's unforged_ticket = [@layout:comb] { ticketer : address ; value : 's ; amount : nat }

type ('p, 's) module_contract = ('p * 's -> operation list * 's) * 's views * dynamic_entrypoints option
type ('p, 's) origination_result =
  { addr : ('p, 's) typed_address
  ; code : ('p, 's) michelson_contract
  ; size : int
  }

type implicit_address = (unit,unit) typed_address

module Test = struct

  let run (type a b) (f : a -> b) (v : a) : michelson_program = [%external ("TEST_RUN", f, v)]
  let eval (type a) (x : a) : michelson_program = run (fun (x : a) -> x) x
  let decompile (type a) (m : michelson_program) : a = [%external ("TEST_DECOMPILE", m)]


  let compile_value (type a) (x : a) : michelson_program = eval x
  let get_total_voting_power (_u : unit) : nat = [%external ("TEST_GET_TOTAL_VOTING_POWER", ())]
  let failwith (type a b) (v : a) : b = [%external ("TEST_FAILWITH", v)]
  let to_contract (type p s) (t : (p, s) typed_address) : p contract = [%external ("TEST_TO_CONTRACT", t)]
  let set_source (a : address) : unit = [%external ("TEST_SET_SOURCE", a)]
  let cast_address (type a b) (a : address) : (a, b) typed_address = [%external ("TEST_CAST_ADDRESS", a)]
  let to_address (type a b) (c : (a, b) typed_address ) : address = [%external ("TEST_TO_ADDRESS", c)]
  let get_storage (type p s) (t : (p, s) typed_address) : s =
    let s : michelson_program = [%external ("TEST_GET_STORAGE", t)] in
    (decompile s : s)
  let get_storage_of_address (type b) (a : address) : b =
    (* we use unit bellow because we don't want inference to force useless annotations *)
    let a : (unit, b) typed_address = cast_address a in
    get_storage a
  let get_balance_of_address (a : address) : tez = [%external ("TEST_GET_BALANCE", a)]
  let get_balance (type p s) (a : (p, s) typed_address) : tez =
    [%external ("TEST_GET_BALANCE", to_address a)]
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
  let bake_until_n_cycle_end (n : nat) : unit = [%external ("TEST_BAKE_UNTIL_N_CYCLE_END", n)]
  let get_time (_u : unit) : timestamp = Tezos.get_now ()
  let register_delegate (kh : key_hash) : unit = [%external ("TEST_REGISTER_DELEGATE", kh)]
  let stake (kh : key_hash) (t : tez) : unit = [%external ("TEST_STAKE", kh, t)]
  let register_constant (m : michelson_program) : string = [%external ("TEST_REGISTER_CONSTANT", m)]
  let to_typed_address (type a b) (c : a contract) : (a, b) typed_address = [%external ("TEST_TO_TYPED_ADDRESS", c)]
  let constant_to_michelson_program (s : string) : michelson_program = [%external ("TEST_CONSTANT_TO_MICHELSON", s)]
  let parse_michelson (s : string) : michelson_program = [%external ("TEST_CONSTANT_TO_MICHELSON", s)]
  let restore_context (u : unit) : unit = [%external ("TEST_POP_CONTEXT", u)]
  let save_context (u : unit) : unit = [%external ("TEST_PUSH_CONTEXT", u)]
  let drop_context (u : unit) : unit = [%external ("TEST_DROP_CONTEXT", u)]
  let to_string (type a) (v : a) : string = [%external ("TEST_TO_STRING", v, 0)]
  let to_json (type a) (v : a) : string = [%external ("TEST_TO_STRING", v, 1)]
  let to_debugger_json (type a) (v : a) : string = [%external ("TEST_TO_STRING", v, 2)]
  let set_baker_policy (bp : test_baker_policy) : unit = [%external ("TEST_SET_BAKER", bp)]
  let set_baker (a : address) : unit = set_baker_policy (By_account a)
  let size (type p s) (c : (p,s) michelson_contract) : int = [%external ("TEST_SIZE", c)]
  let compile_contract (type p s) (f : p * s -> operation list * s) : (p,s) michelson_contract =
    let no_vs : s views = [%external ("TEST_NIL_VIEWS", ())] in
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT", f, no_vs)] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]
  let read_contract_from_file (type p s) (fn : string) : (p,s) michelson_contract = [%external ("TEST_READ_CONTRACT_FROM_FILE", fn)]
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
  let transfer (type p s) (a : (p,s) typed_address) (s : p) (t : tez) : test_exec_result =
    let a = to_contract a in
    let s : michelson_program = eval s in
    [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", a, (None : string option), s, t)]
  let transfer_exn (type p s) (a : (p,s) typed_address) (s : p) (t : tez) : nat =
    let a = to_contract a in
    let s : michelson_program = eval s in
    [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS_EXN", a, (None : string option), s, t)]
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
    let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
    let s : michelson_program = eval s in
    [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", c, e, s, t)]
  let transfer_to_contract_exn (type p) (c : p contract) (s : p) (t : tez) : nat =
      let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
      let s : michelson_program = eval s in
      [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS_EXN", c, e, s, t)]
  let michelson_equal (m1 : michelson_program) (m2 : michelson_program) : bool = m1 = m2
  let to_entrypoint (type a b c) (s : string) (t : (a, b) typed_address) : c contract =
    let s = if String.length s > 0n then
              if String.sub 0n 1n s = "%" then
                let () = log "WARNING: Test.to_entrypoint: automatically removing starting %" in
                String.sub 1n (abs (String.length s - 1)) s
	      else s
	    else s in
    [%external ("TEST_TO_ENTRYPOINT", s, t)]
  let storage_with_dynamic_entrypoints (type p s s2) ((_, _, init_opt) : (p, s) module_contract) (s:s2) =
    type t = [@layout comb] { storage : s2 ; dynamic_entrypoints : dynamic_entrypoints} in
    match init_opt with
    | Some dynamic_entrypoints ->
      ({storage = s ; dynamic_entrypoints } : t)
    | None -> failwith "Your contract do not have dynamic entrypoints"
  let originate_contract (type p s) (c : (p,s) michelson_contract) (s : s) (t : tez) : (p,s) typed_address =
    let s = eval s in
    [%external ("TEST_ORIGINATE", c, s, t)]
  let compile_contract_with_views (type p s) (f : p * s -> operation list * s) (vs : s views) : (p,s) michelson_contract =
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT", f, vs)] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]
  let originate (type p s) ((f, vs, _) : (p, s) module_contract) (s : s) (t : tez) : (p, s) origination_result =
    let code = compile_contract_with_views f vs in
    let addr = originate_contract code s t in
    let size = size code in
    { addr ; code ; size }
  let compile_contract_from_file (type p s) (fn : string) : (p,s) michelson_contract =
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT_FROM_FILE", fn, (None : nat option))] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]
  let originate_from_file (type p s) (fn : string) (s : s)  (t : tez) : (p, s) origination_result =
    let code = compile_contract_from_file fn in
    let addr = originate_contract code s t in
    let size = size code in
    { addr ; code ; size }
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
  let originate_from_file_and_mutate (type b p s) (fn : string) (s : s) (t : tez)
                                     (tester : (p,s) typed_address * (p,s) michelson_contract * int -> b) : (b * mutation) option =
    let wrap_tester (v : (p,s) michelson_contract) : b =
      let f = [%external ("TEST_COMPILE_AST_CONTRACT", v)] in
      let a = originate_contract f s t in
      let c = size f in
      tester (a, f, c) in
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT_FROM_FILE", fn, (None : nat option))] in
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
  let originate_from_file_and_mutate_all (type b p s) (fn : string) (s : s) (t : tez)
                                         (tester : (p,s) typed_address * (p,s) michelson_contract * int -> b) : (b * mutation) list =
    let wrap_tester (v : (p,s) michelson_contract) : b =
      let f = [%external ("TEST_COMPILE_AST_CONTRACT", v)] in
      let a = originate_contract f s t in
      let c = size f in
      tester (a, f, c) in
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT_FROM_FILE", fn, (None : nat option))] in
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
  let originate_module_and_mutate (type p s b) ((f, vs, _) : (p, s) module_contract) (s : s) (t : tez)
                                  (tester : (p, s) typed_address -> (p,s) michelson_contract -> int -> b) : (b * mutation) option =
    let wrap_tester (v : (p,s) michelson_contract) : b =
      let f = [%external ("TEST_COMPILE_AST_CONTRACT", v)] in
      let a = originate_contract f s t in
      let c = size f in
      tester a f c in
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT", f, vs)] in
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
  let originate_and_mutate_all (type p s b) ((f, vs, _) : (p, s) module_contract) (s : s) (t : tez)
                                      (tester : (p, s) typed_address -> (p,s) michelson_contract -> int -> b) : (b * mutation) list =
    let wrap_tester (v : (p,s) michelson_contract) : b =
      let f = [%external ("TEST_COMPILE_AST_CONTRACT", v)] in
      let a = originate_contract f s t in
      let c = size f in
      tester a f c in
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT", f, vs)] in
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

  [@private] let compare (type a) (lhs : a) (rhs : a) : int = [%external ("TEST_COMPARE", lhs, rhs)]

  let equal (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs = 0
  let not_equal (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs <> 0
  let greater (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs > 0
  let less (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs < 0
  let greater_or_equal (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs >= 0
  let less_or_equal (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs <= 0

  let create_chest (b : bytes) (n : nat) : chest * chest_key = [%external ("TEST_CREATE_CHEST", b, n)]
  let create_chest_key (c : chest) (n : nat) : chest_key = [%external ("TEST_CREATE_CHEST_KEY", c, n)]

  module Proxy_ticket = struct
    [@private] let proxy_transfer_contract (type vt whole_p)
        (mk_param : vt ticket -> whole_p)
        (p        : (vt * nat) * address)
        (()       : unit)
        : operation list * unit =
      let ((v,amt),dst_addr) = p in
      let ticket = Option.unopt (Tezos.create_ticket v amt) in
      let tx_param = mk_param ticket in
      let c : whole_p contract = Tezos.get_contract_with_error dst_addr "Testing proxy: you provided a wrong address" in
      let op = Tezos.transaction tx_param 1mutez c in
      [op], ()

    [@private] let proxy_originate_contract (type vt whole_s vp)
        (mk_storage : vt ticket -> whole_s)
        (main       : vp -> whole_s -> operation list * whole_s)
        (p          : (vt * nat))
        (_          : address option)
        : operation list * address option =
      let (v,amt) = p in
      let ticket = Option.unopt (Tezos.create_ticket v amt) in
      let init_storage : whole_s = mk_storage ticket in
      let op,addr = Tezos.create_contract main (None: key_hash option) 0mutez init_storage in
      [op], Some addr

    [@private] let originate_from_function (type p s)
        (f : p -> s -> operation list * s)
        (s : s)
        (t : tez)
        : (p, s) typed_address =
      let code = compile_contract (uncurry f) in
      originate_contract code s t
    type 'v proxy_address = (('v * nat) * address , unit) typed_address

    let init_transfer (type vt whole_p) (mk_param: vt ticket -> whole_p) : vt proxy_address =
      let proxy_transfer : (vt * nat) * address -> unit -> operation list * unit =
        proxy_transfer_contract mk_param
      in
      originate_from_function proxy_transfer () 1tez

    let transfer (type vt)
        (taddr_proxy : vt proxy_address)
        (info        : (vt * nat) * address) : test_exec_result =
      let ticket_info, dst_addr = info in
      transfer_to_contract (to_contract taddr_proxy) (ticket_info , dst_addr) 1mutez

    let originate (type vt whole_s vp)
        (ticket_info : vt * nat)
        (mk_storage  : vt ticket -> whole_s)
        (contract    : vp -> whole_s -> operation list * whole_s)
        : (vp,whole_s) typed_address =
      let proxy_origination : vt * nat -> address option -> operation list * address option =
        proxy_originate_contract mk_storage contract
      in
      let taddr = originate_from_function proxy_origination (None : address option) 1tez in
      let _ = transfer_exn taddr ticket_info 0tez in
      match get_storage taddr with
      | Some addr -> (cast_address addr : (vp,whole_s) typed_address)
      | None -> failwith "internal error"

    let get_storage (type p s s2) (t : (p, s) typed_address) : s2 =
      let s : michelson_program = [%external ("TEST_GET_STORAGE", t)] in
      (decompile s : s2)
  end
  module Next = struct
    module Mutation = struct
      let func = mutation_test
      let from_file = originate_from_file_and_mutate
      let contract = originate_module_and_mutate
      module All = struct
          let func = mutation_test_all
          let from_file = originate_from_file_and_mutate_all
          let contract = originate_and_mutate_all
      end
      let value = mutate_value
      let save = save_mutation
    end
    module PBT = PBT
    module State = struct
      let restore = restore_context
      let save = save_context
      let drop = drop_context
      let reset = reset_state
      let reset_at = reset_state_at
      let register_delegate = register_delegate
      let register_constant = register_constant
    end
    module Account = struct
      let alice () = nth_bootstrap_account 0
      let bob () = nth_bootstrap_account 1
      let carol () = nth_bootstrap_account 2
      let dan () = nth_bootstrap_account 3
      let add = add_account
      let address (n : nat) = nth_bootstrap_account (int n)
      type info = { addr: address; pk: key; sk: string }
      let info (n : nat) : info =
        let (addr, pk, sk) = get_bootstrap_account n in
	{ addr ; pk ; sk }
    end
    module Compare = struct
      let eq = equal
      let neq = not_equal
      let gt = greater
      let lt = less
      let ge = greater_or_equal
      let le = less_or_equal
    end
    module Michelson = struct
      let run = run
      let eval = eval
      let decompile = decompile
      let parse = parse_michelson
      module Contract = struct
        let compile = compile_contract
	let compile_with_views = compile_contract_with_views
	let size = size
	let from_file = read_contract_from_file
      end
    end
    module IO = struct
      let print = print
      let println = println
      let eprint = eprint
      let log = log
      let set_test_print = set_print_values
      let unset_test_print = unset_print_values
    end
    module Assert = struct
      let assert = assert
      let some = assert_some
      let none = assert_none
      module Error = struct
        let assert = assert_with_error
        let some_with_error = assert_some_with_error
        let none_with_error = assert_none_with_error
      end
    end
    module String = struct
      let chr = chr
      let nl = nl
      let show = to_string
      let json = to_json
      let debugger_json = to_debugger_json
    end
    module Ticket = struct
      module Proxy = Proxy_ticket
    end
    module Originate = struct
      type ('p, 's) origination_result =
        { taddr : ('p, 's) typed_address
        ; code : ('p, 's) michelson_contract
        ; size : int
        }
      let contract (type p s) (c : (p, s) module_contract) (s : s) (t : tez) : (p, s) origination_result =
        let { addr; code ; size } = originate c s t in
        let taddr = addr in
        { taddr ; code ; size }
      let from_file (type p s) (fn : string) (s : s)  (t : tez) : (p, s) origination_result =
        let { addr ; code ; size } = originate_from_file fn s t in
	let taddr = addr in
	{ taddr ; code ; size }
    end
    module Contract = struct
      let transfer = transfer_to_contract
      let transfer_exn = transfer_to_contract_exn
    end
    module Typed_address = struct
      let transfer = transfer
      let transfer_exn = transfer_exn
      let get_storage = get_storage
    end
    module Address = struct
      let get_balance = get_balance_of_address
      let to_typed_address = cast_address
    end
    let originate = Originate.contract
    include Typed_address
  end
end