(* LIGO standard library *)

(* Basic types *)

type string = "%constant:string"
type bytes = "%constant:bytes"
type int = "%constant:int"
type nat = "%constant:nat"
type unit = "%constant:unit"

(* Michelson types lifted into LIGO *)

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

(* External (custom) types. These are hardcoded type-level functions
   when the result type of a function depends on the type of the
   arguments. *)

type external_ediv = "%constant:external_ediv"
type external_and = "%constant:external_and"
type external_or = "%constant:external_or"
type external_xor = "%constant:external_xor"
type external_lsl = "%constant:external_lsl"
type external_lsr = "%constant:external_lsr"
type external_bytes = "%constant:external_bytes"

(* Useful types *)

(** Type of entrypoints *)
type ('param, 'storage) entrypoint =
  'param -> 'storage -> operation list * 'storage

(** display-only-for-cameligo
  Type of the booleans. Note: Values `true` and `false` are
  predefined constants such that `true` equals `True` and `false`
  equals `False`. *)
(** display-only-for-jsligo
  Type of the booleans. Note: Values `true` and `false` are
  predefined constants. *)
type bool = False | True

(** display-only-for-cameligo
  Type of optional values. They are useful, for example, when a
  function can fail, but the caller wants to be able to handle the
  failure, which is then denoted by the result `None`. Note:
  Assertions and `failwith` do not allow handling. *)
(** display-only-for-jsligo
  Type of optional values. They are useful, for example, when a
  function can fail, but the caller wants to be able to handle the
  failure, which is then denoted by the result `None()`. Note:
  Assertions and `failwith` do not allow handling. *)
type 'a option = None | Some of 'a

(** Unit type. It is useful for typing side-effects, for example
  failures, some iterators and implicit accounts. *)
[@thunk] [@inline]
let unit : unit = () (* TODO: Remove constant UNIT. *)

(* Useful general-purpose functions *)

(** display-only-for-cameligo
  The call `ignore v` evaluates `v` and ignores its value, returning
  instead the unit value. This is useful when the argument of `ignore`
  performs side-effects. *)
(** display-only-for-jsligo
  The call `ignore(v)` evaluates `v` and ignores its value, returning
  instead the unit value. This is useful when the argument of `ignore`
  performs side-effects. *)
let ignore (type a) (_: a) : unit = ()

(** display-only-for-cameligo
  The call `curry f x y` has the same value as `f (x,y)`. *)
(** display-only-for-jsligo
  The call `curry(f,x,y)` has the same value as `f(x,y)`. *)
let curry (type a b c) (f: a * b -> c) (x: a) (y: b) : c = f (x, y)

(** display-only-for-cameligo
  The call `uncurry f (x,y)` has the same value as `f x y`. *)
(** display-only-for-jsligo
  The call `uncurry(f,[x,y])` has the same value as `f(x)(y)`. *)
let uncurry (type a b c) (f: a -> b -> c) (x,y : a * b) : c = f x y

(** Projecting the first component of a pair *)
let fst (type a b) (x,_ : a * b) : a = x

(** Projecting the second component of a pair. *)
let snd (type a b) (_,y : a * b) : b = y

(** display-only-for-cameligo
  The call `failwith e` terminates the execution with the value `e`,
  standing for an error. Note: Using a string for an error message can
  be costly in terms of size. *)
(** display-only-for-jsligo
  The call `failwith(e)` terminates the execution with the value `e`,
  standing for an error. Note: Using a string for an error message can
  be costly in terms of size. *)
let failwith (type err a) (error: err) : a =
  [%michelson ({| {FAILWITH} |} error : a)]

(** The function `bytes` encodes an integer or a natural number to
  bytes using the big-endian encoding. For integers, negative numbers
  are considered in two's complement representation. *)
let bytes (type a) (value: a) : a external_bytes =
  [%michelson ({| {BYTES} |} value : a external_bytes)]

(* Assertions *)

(** display-only-for-cameligo
  The call `assert_with_error cond error` terminates the execution
  with the string `error` (that is, an error message) if, and only if,
  the boolean condition `cond` is false. *)
(** display-only-for-jsligo
  The call `assert_with_error(cond, error)` terminates the execution
  with the string `error` (that is, an error message) if, and only if,
  the boolean condition `cond` is false. *)
let assert_with_error (condition: bool) (error: string) : unit =
  if condition then () else failwith error

(** display-only-for-cameligo
  The call `assert cond` terminates the execution with the string
  `"failed assertion"` if, and only if, the boolean condition `cond`
  is false. *)
(** display-only-for-jsligo
  The call `assert(cond)` terminates the execution with the string
  `"failed assertion"` if, and only if, the boolean condition `cond`
  is false. *)
[@inline]
let assert (condition: bool) : unit =
  assert_with_error condition "failed assertion"

(** display-only-for-cameligo
  The call `assert_some_with_error opt err` terminates the execution
  with the string `err` (that is, an error message) if, and only if,
  `opt` is `None`. *)
(** display-only-for-jsligo
  The call `assert_some_with_error(opt, err)` terminates the execution
  with the string `err` (that is, an error message) if, and only if,
  `opt` is `None()`. *)
let assert_some_with_error (type a) (opt: a option) (err: string) : unit =
  match opt with
  | None -> failwith err
  | Some _ -> ()

(** display-only-for-cameligo
  The call `assert_some opt` terminates the execution with the
  string `"failed assert some"` if, and only if, `opt` is `None`. *)
(** display-only-for-jsligo
  The call `assert_some(opt)` terminates the execution with the
  string `"failed assert some"` if, and only if, `opt` is `None()`. *)
[@inline]
let assert_some (type a) (opt: a option) : unit =
  assert_some_with_error opt "failed assert some"

(** display-only-for-cameligo
  The call `assert_none_with_error opt err` terminates the execution
  with the string `err` (that is, an error message) if, and only if,
  `opt` is an optional value different from `None`. *)
(** display-only-for-jsligo
  The call `assert_none_with_error(opt, err)` terminates the execution
  with the string `err` (that is, an error message) if, and only if,
  `opt` is an optional value different from `None()`. *)
let assert_none_with_error (type a) (opt: a option) (err: string) : unit =
  match opt with
  | None -> ()
  | Some _ -> failwith err

(** display-only-for-cameligo
  The call `assert_none opt` terminates the execution with the string
  `"failed assert none"` if, and only if, `opt` is not `None`. *)
(** display-only-for-jsligo
  The call `assert_none(opt)` terminates the execution with the string
  `"failed assert none"` if, and only if, `opt` is not `None()`. *)
[@inline]
let assert_none (type a) (opt: a option) : unit =
  assert_none_with_error opt "failed assert none"

(* Arithmetics *)

(** display-only-for-cameligo
    The call `abs i` is the absolute value of `i`. *)
(** display-only-for-jsligo
    The call `abs(i)` is the absolute value of `i`. *)
let abs (integer: int) : nat =
  [%michelson ({| {ABS} |} integer : nat)]

(** display-only-for-cameligo
  The call `is_nat i` is `Some n`, where `n` is the absolute
  value of `i`, if, and only if, `i` is positive or zero. *)
(** display-only-for-jsligo
  The call `is_nat(i)` is `Some(n)`, where `n` is the absolute
  value of `i`, if, and only if, `i` is positive or zero. *)
let is_nat (integer: int) : nat option =
  [%michelson ({| {ISNAT} |} integer : nat option)]

(** display-only-for-cameligo
  The call `int v` casts the value `v` to an integer.

  For natural numbers, the function `int` is the identity cast from
  `nat` to `int`. For BLS12-381 field elements, the returned value is
  always between 0 (inclusive) and the order of the BLS12-381 field
  (exclusive). For bytes, the function `int` decodes the bytes using
  the big-endian encoding, where negative numbers are considered in
  two's complement representation. *)
(** display-only-for-jsligo
  The call `int(v)` casts the value `v` to an integer.

  For natural numbers, the function `int` is the identity cast from
  `nat` to `int`. For BLS12-381 field elements, the returned value is
  always between 0 (inclusive) and the order of the BLS12-381 field
  (exclusive). For bytes, the function `int` decodes the bytes using
  the big-endian encoding, where negative numbers are considered in
  two's complement representation. *)
let int (type a) (value: a) : int =
  [%michelson ({| {INT} |} value : int)]

(** display-only-for-cameligo
  The call `nat b` casts the bytes `b` into a natural number. *)
(** display-only-for-jsligo
  The call `nat(b)` casts the bytes `b` into a natural number. *)
let nat (bytes: bytes) : nat =
  [%michelson ({| {NAT} |} bytes : nat)]

(** display-only-for-cameligo
  The call `ediv z1 z2`, where `z1` and `z2` are either of type
  `int` or `nat`, returns `None` if `z2` is zero; otherwise, it
  returns the pair `(q,r)`, where `q` is the quotient and `r` the
  positive remainder, as is the convention of the mathematical
  Euclidian division. *)
(** display-only-for-jsligo
  The call `ediv(z1, z2)`, where `z1` and `z2` are either of type
  `int` or `nat`, returns `None()` if `z2` is zero; otherwise, it
  returns the pair `[q,r]`, where `q` is the quotient and `r` the
  positive remainder, as is the convention of the mathematical
  Euclidian division. *)
let ediv (type a b) (left: a) (right: b) : (a, b) external_ediv =
  [%michelson ({| {EDIV} |} left right : (a, b) external_ediv)]

(** Tezos-specific functions *)
module Tezos = struct

  (* Addresses *)

  (** display-only-for-cameligo
    The call `get_sender ()` is the address of the contract (that
    is, a smart contract or an implicit account) that initiated the
    current internal transaction. Note that, if transactions have been
    chained, that address could be different from `get_source ()`. *)
  (** display-only-for-jsligo
    The call `get_sender()` is the address of the contract (that
    is, a smart contract or an implicit account) that initiated the
    current internal transaction. Note that, if transactions have been
    chained, that address could be different from `get_source()`. *)
  let get_sender () : address =
    [%michelson ({| {SENDER} |} : address)]

  (** display-only-for-cameligo
    The call `get_source ()` is the address of the implicit account
    that initiated the current transaction. If transactions have been
    chained, that address is different from `get_sender ()`. *)
  (** display-only-for-jsligo
    The call `get_source()` is the address of the implicit account
    that initiated the current transaction. If transactions have been
    chained, that address is different from `get_sender()`. *)
  let get_source () : address =
    [%michelson ({| {SOURCE} |} : address)]

  (** display-only-for-cameligo
    The call `self entrypoint` is the address of the current smart
    contract, that is, the smart contract containing the call. For the
    address of the smart contract actually *executing* the call,
    because it is embedded in a lambda sent to another smart contract,
    use `get_self_address` instead. The string `entrypoint` is the
    name of a valid entrypoint such that `entrypoint` is not
    `"default"`, or the empty string denoting the `"default"`
    entrypoint (which is the root of the smart contract parameter if
    no `"default"` entrypoint is explicitly defined). If the contract
    does not have the specified entrypoint, the call results in an
    type checking error. *)
  (** display-only-for-jsligo
    The call `self(entrypoint)` is the address of the current smart
    contract, that is, the smart contract containing the call. For the
    address of the smart contract actually *executing* the call,
    because it is embedded in a lambda sent to another smart contract,
    use `get_self_address` instead. The string `entrypoint` is the
    name of a valid entrypoint such that `entrypoint` is not
    `"default"`, or the empty string denoting the `"default"`
    entrypoint (which is the root of the smart contract parameter if
    no `"default"` entrypoint is explicitly defined). If the contract
    does not have the specified entrypoint, the call results in an
    type checking error. *)
  [@inline] [@thunk]
  let self (type a) (entrypoint: string) : a contract =
    let _ : a option = [%external ("CHECK_SELF", entrypoint)] in
    [%michelson ({| {SELF (annot $0)} |} entrypoint : a contract)]

  (** display-only-for-cameligo
    The call `get_self_address ()` is the address of the smart
    contract actually executing the call, as a value of type
    `address`. That contract can be different from the one containing
    the call if the call is in a lambda transmitted to another smart
    contract. Therefore, it is assumed that, in general, the type of
    the executing contract is statically unknown, so the return type
    of `get_self_address` is not `'a contract`, but `address`. (See
    `self`.) *)
  (** display-only-for-jsligo
    The call `get_self_address()` is the address of the smart
    contract actually executing the call, as a value of type
    `address`. That contract can be different from the one containing
    the call if the call is in a lambda transmitted to another smart
    contract. Therefore, it is assumed that, in general, the type of
    the executing contract is statically unknown, so the return type
    of `get_self_address` is not `'a contract`, but `address`. (See
    `self`.) *)
  let get_self_address () : address =
    [%michelson ({| {SELF_ADDRESS} |} : address)]

  (** display-only-for-cameligo
    The call `address contract` casts the address of the smart
    contract `contract` into the more general value of type
    `address`. *)
  (** display-only-for-jsligo
    The call `address(contract)` casts the address of the smart
    contract `contract` into the more general value of type
    `address`. *)
  let address (type a) (contract_addr: a contract) : address =
    [%michelson ({| {ADDRESS} |} contract_addr : address)]

  (** display-only-for-cameligo
    The call `implicit_account kh` casts the public key hash `kh`
    into the address of its implicit account. Note that addresses of
    implicit accounts always have the type `unit contract`. *)
  (** display-only-for-jsligo
    The call `implicit_account(kh)` casts the public key hash `kh`
    into the address of its implicit account. Note that addresses of
    implicit accounts always have the type `contract<unit>`. *)
  let implicit_account (kh: key_hash) : unit contract =
    [%michelson ({| {IMPLICIT_ACCOUNT} |} kh : unit contract)]

  (* Contracts and operations *)

  (** display-only-for-cameligo
    The call `get_contract_opt addr` casts the address `addr` into
    that of a contract address, if such contract exists. The value of
    the call is `None` if no such contract exists, otherwise `Some
    contract`, where `contract` is the contract's address. Note: The
    address of an implicit account has type `unit contract`. *)
  (** display-only-for-jsligo
    The call `get_contract_opt(addr)` casts the address `addr` into
    that of a contract address, if such contract exists. The value of
    the call is `None()` if no such contract exists, otherwise `Some
    contract`, where `contract` is the contract's address. Note: The
    address of an implicit account has type `unit contract`. *)
  [@inline] [@thunk]
  let get_contract_opt (type param) (addr: address) : param contract option =
    [%michelson ({| {CONTRACT (typeopt $0)} |} (None : param option) addr
                 : param contract option)]

  (** display-only-for-cameligo
    The call `get_contract_with_error addr error` casts the address
    `addr` into that of a contract address, if such contract
    exists. If not, the execution fails with the error message
    `error`. *)
  (** display-only-for-jsligo
    The call `get_contract_with_error(addr, error)` casts the address
    `addr` into that of a contract address, if such contract
    exists. If not, the execution fails with the error message
    `error`. *)
  let get_contract_with_error (type param) (addr: address) (error: string)
    : param contract =
    match get_contract_opt addr with
    | None -> failwith error
    | Some contract_addr -> contract_addr

  (** display-only-for-cameligo
    The call `get_contract addr` casts the address `addr` into that
    of a smart contract address, if such contract exists. The call
    fails with the message `"bad address for get_contract"` if no
    such smart contract exists. Note: The address of an implicit
    account has type `unit contract`. *)
  (** display-only-for-jsligo
    The call `get_contract(addr)` casts the address `addr` into that
    of a smart contract address, if such contract exists. The call
    fails with the message `"bad address for get_contract"` if no
    such smart contract exists. Note: The address of an implicit
    account has type `contract<unit>`. *)
  [@inline] [@thunk]
  let get_contract (type param) (addr: address) : param contract =
    get_contract_with_error addr "bad address for get_contract"

  (** display-only-for-cameligo
    The call `get_entrypoint_opt entrypoint addr` has the same
    behaviour as `get_contract_opt addr`, with the additional
    constraint that the contract must have an entrypoint named
    `entrypoint`. In other words, `get_entrypoint_opt entrypoint addr`
    casts the address `addr` into that of a smart contract
    address, if such contract exists and has an entrypoint named
    `entrypoint`. The value of the call is `None` if no such smart
    contract exists, otherwise `Some contract`, where `contract` is
    the smart contract's address. Note: The address of an implicit
    account has type `unit contract`. *)
  (** display-only-for-jsligo
    The call `get_entrypoint_opt(entrypoint, addr)` has the same
    behaviour as `get_contract_opt(addr)`, with the additional
    constraint that the contract must have an entrypoint named
    `entrypoint`. In other words, `get_entrypoint_opt(entrypoint, addr)`
    casts the address `addr` into that of a smart contract
    address, if such contract exists and has an entrypoint named
    `entrypoint`. The value of the call is `None()` if no such smart
    contract exists, otherwise `Some(contract)`, where `contract` is
    the smart contract's address. Note: The address of an implicit
    account has type `contract<unit>`. *)
  [@inline] [@thunk]
  let get_entrypoint_opt (type param) (entrypoint: string) (addr: address)
    : param contract option =
    let () = [%external ("CHECK_ENTRYPOINT", entrypoint)] in
    [%michelson ({| {CONTRACT (annot $0) (typeopt $1)} |}
                 entrypoint (None : param option) addr
                 : param contract option)]

  (** display-only-for-cameligo
    The call `get_entrypoint entrypoint addr` casts the address
    `addr` into that of a smart contract address, if such contract
    exists and has an entrypoint named `entrypoint`. If no such smart
    contract exists, the execution fails with the error message
    `"bad address for get_entrypoint"`. Note: The address of an implicit
    account has type `unit contract`. *)
  (** display-only-for-jsligo
    The call `get_entrypoint(entrypoint, addr)` casts the address
    `addr` into that of a smart contract address, if such contract
    exists and has an entrypoint named `entrypoint`. If no such smart
    contract exists, the execution fails with the error message
    `"bad address for get_entrypoint"`. Note: The address of an implicit
    account has type `contract<unit>`. *)
  [@inline] [@thunk]
  let get_entrypoint (type param) (entrypoint: string) (addr: address)
    : param contract =
    match get_entrypoint_opt entrypoint addr with
    | None -> failwith "bad address for get_entrypoint"
    | Some contract_addr -> contract_addr

  (** display-only-for-cameligo
    The call `create_contract c e a s` returns a contract creation
    operation (origination) for the entrypoint `e` (as a function)
    with optional delegate `d`, initial amount `a` and initial
    storage `s`, together with the address of the created
    contract. Note that the created contract cannot be called
    immediately afterwards (that is, `get_contract_opt` on that
    address would return `None`), as the origination must be
    performed successfully first, for example by calling a proxy
    contract or itself. *)
  (** display-only-for-jsligo
    The call `create_contract(c,e,a,s)` returns a contract creation
    operation (origination) for the entrypoint `e` (as a function)
    with optional delegate `d`, initial amount `a` and initial
    storage `s`, together with the address of the created
    contract. Note that the created contract cannot be called
    immediately afterwards (that is, `get_contract_opt` on that
    address would return `None()`), as the origination must be
    performed successfully first, for example by calling a proxy
    contract or itself. *)
  [@inline] [@thunk]
  let create_contract
    (type param storage)
    (entrypoint: (param, storage) entrypoint)
    (delegate: key_hash option)
    (amount: tez)
    (storage: storage)
    : operation * address =
    [%external ("CREATE_CONTRACT",
                uncurry entrypoint, delegate, amount, storage)]

  (** display-only-for-cameligo
    The call `set_delegate d` evaluates in an operation that sets
    the delegate of the current smart contract to be `d`, where `d` is
    an optional key hash. If `None`, the delegation is withdrawn. If
    the contract has no delegation, then no change occurs. If `d` is
    `Some kh`, where `kh` is the key hash of a registered delegate
    that is not the current delegate of the contract, then this
    operation sets the delegate of the contract to this registered
    delegate. A failure occurs if `kh` is the current delegate of the
    contract or if `kh` is not a registered delegate. However, the
    instruction in itself does not fail; it produces an operation that
    will fail when applied. *)
  (** display-only-for-jsligo
    The call `set_delegate(d)` evaluates in an operation that sets
    the delegate of the current smart contract to be `d`, where `d` is
    an optional key hash. If `None()`, the delegation is withdrawn. If
    the contract has no delegation, then no change occurs. If `d` is
    `Some(kh)`, where `kh` is the key hash of a registered delegate
    that is not the current delegate of the contract, then this
    operation sets the delegate of the contract to this registered
    delegate. A failure occurs if `kh` is the current delegate of the
    contract or if `kh` is not a registered delegate. However, the
    instruction in itself does not fail; it produces an operation that
    will fail when applied. *)
  let set_delegate (delegate: key_hash option) : operation =
    [%michelson ({| {SET_DELEGATE} |} delegate : operation)]

  (** display-only-for-cameligo
    The call `transaction param amount contract_addr` evaluates in
    an operation that will send the amount `amount` in mutez to the
    contract at the valid address `contract_addr`, with parameter
    `param`. If the contract is an implicit account, the parameter
    must be `unit`. *)
  (** display-only-for-jsligo
    The call `transaction(param, amount, contract_addr)` evaluates in
    an operation that will send the amount `amount` in mutez to the
    contract at the valid address `contract_addr`, with parameter
    `param`. If the contract is an implicit account, the parameter
    must be `unit`. *)
  let transaction
    (type param) (param: param) (amount: tez) (contract_addr: param contract)
    : operation =
    [%michelson
      ({| {TRANSFER_TOKENS} |} param amount contract_addr : operation)]

  (** display-only-for-cameligo
    The call `call_view v p a` calls the view `v` with parameter
    `param` at the contract whose address is `a`. The value returned
    is `None` if the view does not exist, or has a different type of
    parameter, or if the contract does not exist at that
    address. Otherwise, it is `Some v`, where `v` is the return value
    of the view. Note: the storage of the view is the same as when the
    execution of the contract calling the view started.*)
  (** display-only-for-jsligo
    The call `call_view(v, p, a)` calls the view `v` with parameter
    `param` at the contract whose address is `a`. The value returned
    is `None()` if the view does not exist, or has a different type of
    parameter, or if the contract does not exist at that
    address. Otherwise, it is `Some(v)`, where `v` is the return value
    of the view. Note: the storage of the view is the same as when the
    execution of the contract calling the view started. *)
  [@inline] [@thunk]
  let call_view
    (type param return) (view: string) (param: param) (addr: address)
    : return option =
    let () = [%external ("CHECK_CALL_VIEW_LITSTR", view)]
    in [%michelson ({| {VIEW (litstr $0) (typeopt $1)} |}
                    view (None : return option) param addr
                    : return option)]

  (* Tickets *)

  (** display-only-for-cameligo
    The call `create_ticket v a` creates a ticket with value `v` and
    amount `a`. If the creation is a success, the value `Some t` is
    returned, where `t` is the ticket; otherwise, `None` is the
    result. Note: Tickets cannot be duplicated. *)
  (** display-only-for-jsligo
    The call `create_ticket(v, a)` creates a ticket with value `v` and
    amount `a`. If the creation is a success, the value `Some(t)` is
    returned, where `t` is the ticket; otherwise, `None()` is the
    result. Note: Tickets cannot be duplicated. *)
  let create_ticket (type a) (value: a) (amount: nat) : a ticket option =
    [%michelson ({| {TICKET} |} value amount : a ticket option)]

  (** display-only-for-cameligo
    The call `split_ticket t (a1, a2)` results in a pair of tickets
    `t1` and `t2` such that the former owns the amount `a1` and the
    later `a2`. More precisely, the value of the call is
    `Some (t1, t2)` because signifying to the callee the failure of
    the splitting is achieved by returning the value `None`. *)
  (** display-only-for-jsligo
    The call `split_ticket(t, [a1, a2])` results in a pair of tickets
    `t1` and `t2` such that the former owns the amount `a1` and the
    later `a2`. More precisely, the value of the call is
    `Some([t1, t2])` because signifying to the callee the failure of
    the splitting is achieved by returning the value `None()`. *)
  let split_ticket (type a) (ticket: a ticket) (amounts: nat * nat)
    : (a ticket * a ticket) option =
    [%michelson ({| {SPLIT_TICKET} |} ticket amounts
                 : (a ticket * a ticket) option)]

  (** display-only-for-cameligo
    The call `join_tickets (t1, t2)` joins the tickets `t1` and
    `t2`, which must have the same type of value. *)
  (** display-only-for-jsligo
    The call `join_tickets(t1, t2)` joins the tickets `t1` and
    `t2`, which must have the same type of value. *)
  let join_tickets (type a) (tickets: a ticket * a ticket) : a ticket option =
    [%michelson ({| {JOIN_TICKETS} |} tickets : a ticket option)]

  (** display-only-for-cameligo
    The call `read_ticket t` returns `t` itself and the contents of
    `t` which is a pair `(address, (value, amount))`, where `address` is
    the address of the smart contract that created it. *)
  (** display-only-for-jsligo
    The call `read_ticket(t)` returns `t` itself and the contents of
    `t` which is a pair `[address, [value, amount]]`, where `address` is
    the address of the smart contract that created it. *)
  let read_ticket (type a) (ticket: a ticket)
    : (address * (a * nat)) * a ticket =
    [%michelson ({| {READ_TICKET; PAIR} |} ticket
                 : (address * (a * nat)) * a ticket)]

  (* Sapling *)

  (** The evaluation of the constant `sapling_empty_state` is an empty
    sapling state, that is, no one can spend tokens from it. *)
  [@inline] [@thunk]
  let sapling_empty_state (type sap_t) : sap_t sapling_state =
    [%michelson ({| {SAPLING_EMPTY_STATE (typeopt $0)} |}
                 (None : sap_t option)
                 : sap_t sapling_state)]

  (** display-only-for-cameligo
    The call `sapling_verify_update trans state`, where the
    transaction `trans` can be applied to the state `state`, returns
    `Some (data, (delta, new_state))`, where `data` is the bound data
    (as bytes), `delta` is the difference between the outputs and the
    inputs of the transaction, and `new_state` is the updated
    state. *)
  (** display-only-for-jsligo
    The call `sapling_verify_update(trans, state)`, where the
    transaction `trans` can be applied to the state `state`, returns
    `Some ([data, [delta, new_state]])`, where `data` is the bound data
    (as bytes), `delta` is the difference between the outputs and the
    inputs of the transaction, and `new_state` is the updated
    state. *)
  [@inline] [@thunk]
  let sapling_verify_update
    (type sap_a)
    (trans: sap_a sapling_transaction)
    (state: sap_a sapling_state)
    : (bytes * (int * sap_a sapling_state)) option =
    [%michelson ({| {SAPLING_VERIFY_UPDATE} |} trans state
                 : (bytes * (int * sap_a sapling_state)) option)]

  (* Events *)

  (** display-only-for-cameligo
    The call `emit event_tag event_type` evaluates in an operation
    that will write an event into the transaction receipt after the
    successful execution of this contract. The event is annotated by
    the string `event_tag` if it is not empty. The argument
    `event_type` is used only to specify the type of data attachment. *)
  (** display-only-for-jsligo
    The call `emit event_tag(event_type)` evaluates in an operation
    that will write an event into the transaction receipt after the
    successful execution of this contract. The event is annotated by
    the string `event_tag` if it is not empty. The argument
    `event_type` is used only to specify the type of data attachment. *)
  [@inline] [@thunk]
  let emit (type event_type) (event_tag: string) (event_type: event_type)
    : operation =
    let () = [%external ("CHECK_EMIT_EVENT", event_tag, event_type)] in
    [%michelson ({| {EMIT (annot $0) (typeopt $1)} |}
                 event_tag (None : event_type option) event_type
                 : operation)]

  (* Time-lock *)

  (** The function [open_chest] opens a timelocked chest given its key
    and the time. The result is a byte option depending if the opening
    is correct or not. *)
  let open_chest (key: chest_key) (chest: chest) (time: nat) : bytes option =
    [%michelson ({| {OPEN_CHEST} |} key chest time : bytes option)]

  (* Miscellanea *)

  (** display-only-for-cameligo
    The call `get_balance ()` returns the balance in mutez of the
    account associated to the currently executed smart contract,
    including any mutez added by the calling transaction. *)
  (** display-only-for-jsligo
    The call `get_balance()` returns the balance in mutez of the
    account associated to the currently executed smart contract,
    including any mutez added by the calling transaction. *)
  let get_balance () : tez =
    [%michelson ({| {BALANCE} |} : tez)]

  (** display-only-for-cameligo
    The call `get_amount ()` returns the amount in mutez of the
    current transaction. *)
  (** display-only-for-jsligo
    The call `get_amount()` returns the amount in mutez of the
    current transaction. *)
  let get_amount () : tez =
    [%michelson ({| {AMOUNT} |} : tez)]

  (** display-only-for-cameligo
    The call `get_now ()` returns the minimal injection time for the
    current block, namely the block whose application triggered this
    execution. The minimal injection time constitutes an estimate of
    the moment when the current block is injected, hence the name
    "now". *)
  (** display-only-for-jsligo
    The call `get_now()` returns the minimal injection time for the
    current block, namely the block whose application triggered this
    execution. The minimal injection time constitutes an estimate of
    the moment when the current block is injected, hence the name
    "now". *)
  let get_now () : timestamp =
    [%michelson ({| {NOW} |} : timestamp)]

  (** display-only-for-cameligo
    The call `get_min_block_time ()` returns the minimal delay
    between two consecutive blocks in the chain. *)
  (** display-only-for-jsligo
    The call `get_min_block_time()` returns the minimal delay
    between two consecutive blocks in the chain. *)
  let get_min_block_time () : nat =
    [%michelson ({| {MIN_BLOCK_TIME} |} : nat)]

  (** display-only-for-cameligo
    The call `get_level ()` returns the current block level. *)
  (** display-only-for-jsligo
    The call `get_level()` returns the current block level. *)
  let get_level () : nat =
    [%michelson ({| {LEVEL} |} : nat)]

  (** display-only-for-cameligo
    The call `get_chain_id ()` returns the identifier of the chain
    on which the smart contract is executed. *)
  (** display-only-for-jsligo
    The call `get_chain_id ()` returns the identifier of the chain
    on which the smart contract is executed. *)
  let get_chain_id () : chain_id =
    [%michelson ({| {CHAIN_ID} |} : chain_id)]

  (** display-only-for-cameligo
    The call `get_total_voting_power ()` returns the total voting
    power of all contracts. The total voting power coincides with the
    sum of the stake of every contract in the voting listings. The
    voting listings is calculated at the beginning of every voting
    period. *)
  (** display-only-for-jsligo
    The call `get_total_voting_power()` returns the total voting
    power of all contracts. The total voting power coincides with the
    sum of the stake of every contract in the voting listings. The
    voting listings is calculated at the beginning of every voting
    period. *)
  let get_total_voting_power () : nat =
    [%michelson ({| {TOTAL_VOTING_POWER} |} : nat)]

  (** display-only-for-cameligo
    The call `voting_power contract_kh` returns the voting power of
    a given contract specified by the key hash `contract_kh`. This
    voting power coincides with the weight of the contract in the
    voting listings (that is, the stake) which is calculated at the
    beginning of every voting period. *)
  (** display-only-for-jsligo
    The call `voting_power(contract_kh)` returns the voting power of
    a given contract specified by the key hash `contract_kh`. This
    voting power coincides with the weight of the contract in the
    voting listings (that is, the stake) which is calculated at the
    beginning of every voting period. *)
  let voting_power (kh : key_hash) : nat =
    [%michelson ({| {VOTING_POWER} |} kh : nat)]

  (** display-only-for-cameligo
    The call `never n` is never meant to be executed, as the type
    `never` is inhabited, but to instruct the typechecker that a
    branch in the control flow, for example, in a pattern matching, is
    dead. *)
  (** display-only-for-jsligo
    The call `never(n)` is never meant to be executed, as the type
    `never` is inhabited, but to instruct the typechecker that a
    branch in the control flow, for example, in a pattern matching, is
    dead. *)
  let never (type a) (never: never) : a =
    [%michelson ({| {NEVER} |} never : a)]

  (** display-only-for-cameligo
    The call `pairing_check pairings` verifies that the product of
    pairings of the given list of points `pairings` is equal to 1 in
    the field Fq12. It evaluates in `true` if the list is empty. This
    function can be used to verify if two pairings P1 and P2 are equal
    by verifying P1 * P2^(-1) = 1. *)
  (** display-only-for-jsligo
    The call `pairing_check(pairings)` verifies that the product of
    pairings of the given list of points `pairings` is equal to 1 in
    the field Fq12. It evaluates in `true` if the list is empty. This
    function can be used to verify if two pairings P1 and P2 are equal
    by verifying P1 * P2^(-1) = 1. *)
  let pairing_check (list: (bls12_381_g1 * bls12_381_g2) list) : bool =
    [%michelson ({| {PAIRING_CHECK} |} list : bool)]

  (** display-only-for-cameligo
    The call to `constant hash` returns the value stored on-chain
    whose hash value is `hash` (global constants). This call can fail
    when the contract is originated if the hash is invalid or the
    expansion of the global constant is ill-typed, or too large (gas
    consumption). *)
  (** display-only-for-cameligo
    The call to `constant(hash)` returns the value stored on-chain
    whose hash value is `hash` (global constants). This call can fail
    when the contract is originated if the hash is invalid or the
    expansion of the global constant is ill-typed, or too large (gas
    consumption). *)
  [@inline] [@thunk]
  let constant (type a) (hash: string) : a =
    [%external ("GLOBAL_CONSTANT", hash)]
end

(** Bitwise operations *)
module Bitwise = struct

  (** The call `@and a b` is the conjunction defined on boolean,
    natural number and bytes operands. In the boolean case, the result
    is the logical "and" of the operands. In the natural number and
    bytes cases, the result is the bitwise "and" of the operands.

    The function `@and` is also defined when the left operand is of
    type `int`. Negative numbers are considered in two's complement
    representation, starting with a virtual infinite number of 1s.

    When `@and` is used for bytes operands, the bytes result has the
    same length as the shorter operand. The prefix of the longer
    operand is cut to match with the length of the shorter one before
    taking the bitwise "and". *)
  let @and (type a b) (left: a) (right: b) : (a, b) external_and =
    [%michelson ({| {AND} |} left right : (a, b) external_and)]

  (** The call `@or a b` is the disjunction defined on boolean,
    natural number and bytes operands. In the boolean case, the result
    is the logical "or" of the operands. In the natural number and
    bytes cases, the result is the bitwise "or" of the operands.

    When the function `@or` is used for bytes operands, the result
    bytes has the same length as the longer operand. The shorter
    operand is zero-padded on the left to match with the length of the
    longer one before taking the bitwise "or". *)
  let @or (type a b) (left: a) (right: b) : (a, b) external_xor =
    [%michelson ({| {OR} |} left right : (a, b) external_xor)]

  (** The call `xor a b` is the exclusive disjunction defined on
    boolean, natural number and bytes operands. In the boolean case,
    the result is the logical "exclusive or" of the operands. In the
    natural number and bytes cases, the result is the bitwise "xor" of
    the operands.

    When `xor` is used for bytes operands, the result bytes has the
    same length as the longer operand. The shorter operand is
    zero-padded on the left to match with the length of the longer one
    before taking the bitwise "xor". *)
  let xor (type a b) (left: a) (right: b) : (a, b) external_or =
    [%michelson ({| {XOR} |} left right : (a, b) external_or )]

  (** The function `shift_left` on natural numbers consumes two
    natural numbers and produces the first number logically
    left-shifted by the second number. This instruction is only
    defined if the second number is less than or equal to 256.

    For bytes, the function `shift_left` consumes one byte sequence
    and one natural number, and produces the bytes logically
    left-shifted by the natural number. The vacated bits on the right
    are filled with zeros. The shifted bits are minimally zero-padded
    on the left in order to keep all the original bits, regardless if
    they are 0 or 1: for example, `shift_left 0x1234 1` is `0x002468`,
    instead of `0x2468` (even though in this case no significant bit
    would be lost) or `0x00002468` (where padding is not minimal). The
    length of the bytes returned by `shift_left` is `l + (s + 7) / 8`
    bytes where `l` is the length of the original bytes and `s` is the
    natural number. This instruction is only defined if the second
    number is less than or equal to 64000. *)
  let shift_left (type a b) (left: a) (right: b) : (a, b) external_lsl =
    [%michelson ({| {LSL} |} left right : (a, b) external_lsl)]

  (** The function `shift_right` on natural numbers consumes two
    natural numbers and produces the first number logically
    right-shifted by second number. This function is only defined if
    the second number is less than or equal to 256.

    For bytes, the function `shift_right` consumes one chunk of bytes
    and one natural number and produces the bytes logically
    right-shifted by the natural number. The shifted bits are
    minimally zero-padded on the left. For example, `shift_right
    0x012349 9` is `0x0091`, instead of `0x91` (where the 7 left-most
    bits are lost) or `0x000091` (not minimal padding). The length of
    the returned bytes by `shift_right` is `max 0 (l - s / 8)` bytes,
    where `l` is the length of the original bytes, and `s` is the
    natural number. *)
  let shift_right (type a b) (left: a) (right: b) : (a, b) external_lsr =
    [%michelson ({| {LSR} |} left right : (a, b) external_lsr)]
end

(** The module of optional values *)
module Option = struct

  (** display-only-for-cameligo
    The call `value d opt` is `v` if `opt` is `Some v`, and `d`
    otherwise. *)
  (** display-only-for-jsligo
    The call `value(d, opt)` is `v` if `opt` is `Some(v)`, and `d`
    otherwise. *)
  let value (type a) (default: a) (opt: a option) : a =
    match opt with
    | None -> default
    | Some v -> v

  (** display-only-for-cameligo
    The call `value_with_error err opt` terminates with the error
    `err` if, and only if, `opt` is `None`; otherwise it is `Some v`
    and `v` is returned. *)
  (** display-only-for-jsligo
    The call `value_with_error(err, opt)` terminates with the error
    `err` if, and only if, `opt` is `None()`; otherwise it is `Some(v)`
    and `v` is returned. *)
  let value_with_error (type err a) (error: err) (opt: a option) : a =
    match opt with
    | None -> failwith error
    | Some v -> v

  (** display-only-for-cameligo
    The call `value_exn err opt` terminates with the error `err` if,
    and only if, `opt` is `None`; otherwise it is `Some v` and `v` is
    returned. *)
  (** display-only-for-jsligo
    The call `value_exn(err, opt)` terminates with the error `err` if,
    and only if, `opt` is `None()`; otherwise it is `Some(v)` and `v` is
    returned. *)
  [@inline] [@deprecated "Use `Option.value_with_error` instead."]
  let value_exn (type err a) (error: err) (opt: a option) : a =
    value_with_error error opt

  (** display-only-for-cameligo
    The call `unopt_with_error opt err` terminates with the error
    `err` if, and only if, `opt` is `None`; otherwise it is `Some v`
    and `v` is returned. *)
  (** display-only-for-jsligo
    The call `unopt_with_error(opt, err)` terminates with the error
    `err` if, and only if, `opt` is `None()`; otherwise it is
    `Some(v)` and `v` is returned. *)
  [@inline] [@deprecated "Use `Option.value_with_error` instead."]
  let unopt_with_error (type a) (opt: a option) (error: string) : a =
    value_with_error error opt

  (** display-only-for-cameligo
    The call `unopt opt ` terminates with the string
    `"option is None"` if, and only if, `opt` is `None`; otherwise it is
    `Some v` and `v` is returned.*)
  (** display-only-for-jsligo
    The call `unopt(opt)` terminates with the string
    `"option is None"` if, and only if, `opt` is `None()`; otherwise it is
    `Some(v)` and `v` is returned.*)
  [@inline] [@deprecated "Use `Option.value_with_error` instead."]
  let unopt (type a) (opt: a option) : a =
    value_with_error "option is None" opt

  (** display-only-for-cameligo
    The call `map f opt` is `None` if `opt` is `None`, and
    `Some (f v)` if `opt` is `Some v`. *)
  (** display-only-for-jsligo
    The call `map(f, opt)` is `None()` if `opt` is `None()`, and
    `Some(f(v))` if `opt` is `Some(v)`. *)
  [@thunk]
  let map (type a b) (f: a -> b) (opt: a option) : b option =
    [%external ("OPTION_MAP", f, opt)]

  (** display-only-for-cameligo
    The call `is_none opt` is `true` if, and only if, `opt` is
    `None`. *)
  (** display-only-for-jsligo
    The call `is_none(opt)` is `true` if, and only if, `opt` is
    `None()`. *)
  let is_none (type a) (opt: a option) : bool =
    match opt with
    | None -> true
    | Some _ -> false

  (** display-only-for-cameligo
    The call `is_some opt` is `false` if, and only if, `opt` is
    `None`. *)
  (** display-only-for-jsligo
    The call `is_some(opt)` is `false` if, and only if, `opt` is
    `None()`. *)
  let is_some (type a) (opt: a option) : bool =
    match opt with
    | None -> false
    | Some _ -> true
end

(** Lists *)
module List = struct

  (** The type `t` is an alias for the predefined type `list`. *)
  type 'elt t = 'elt list

  (** display-only-for-cameligo
    The value `empty` is the empty list. It is a synonym for
    `[]`. In some contexts, it is useful to annotate it with its type,
    for example: `(empty : int list)`. *)
  (** display-only-for-jsligo
    The value `empty` is the empty list. It is a synonym for
    `list([])`. In some contexts, it is useful to annotate it with its
    type, for example: `(empty as list<int>)`. *)
  let empty (type elt) : elt t = []

  (** display-only-for-cameligo
      The call `length l` is the number of elements in the list
      `l`. Note: `List.length` is another name for `List.size`. *)
  (** display-only-for-jsligo
      The call `length(l)` is the number of elements in the list
      `l`. Note: `List.length` is another name for `List.size`. *)
  let length (type elt) (list: elt t) : nat =
    [%external ("LIST_SIZE", list)]

  (** display-only-for-cameligo
    The call `size l` is the number of elements in the list `l`. *)
  (** display-only-for-jsligo
    The call `size(l)` is the number of elements in the list `l`. *)
  [@inline]
  let size (type elt) (list: elt t) : nat = length list

  (** display-only-for-cameligo
    The call `head l`, where `l` is a list, is `None` if `l` is
    empty; otherwise, `Some hd`, where `hd` is the head of the list. *)
  (** display-only-for-jsligo
    The call `head(l)`, where `l` is a list, is `None()` if `l` is
    empty; otherwise, `Some(hd)`, where `hd` is the head of the list. *)
  let head (type elt) (list: elt t) : elt option =
    match list with
    | [] -> None
    | head :: _ -> Some head

  (** display-only-for-cameligo
    The call `head_opt l`, where `l` is a list, is `None` if `l` is
    empty; otherwise, `Some hd`, where `hd` is the head of the list. *)
  (** display-only-for-jsligo
    The call `head_opt(l)`, where `l` is a list, is `None()` if `l` is
    empty; otherwise, `Some(hd)`, where `hd` is the head of the list. *)
  [@inline] [@deprecated "Use `List.head` instead."]
  let head_opt (type elt) (list: elt t) : elt option = head list

  (** display-only-for-cameligo
    The call `tail l`, where `l` is a list, is `None` if `l` is
    empty; otherwise, `Some tl`, where `tl` is the tail of the list. *)
  (** display-only-for-jsligo
    The call `tail(l)`, where `l` is a list, is `None()` if `l` is
    empty; otherwise, `Some(tl)`, where `tl` is the tail of the list. *)
  let tail (type elt) (list: elt t) : elt t option =
    match list with
    | [] -> None
    | _ :: tail -> Some tail

  (** display-only-for-cameligo
    The call `tail_opt l`, where `l` is a list, is `None` if `l` is
    empty; otherwise, `Some tl`, where `tl` is the tail of the list. *)
  (** display-only-for-jsligo
    The call `tail_opt(l)`, where `l` is a list, is `None()` if `l` is
    empty; otherwise, `Some(tl)`, where `tl` is the tail of the list. *)
  [@inline] [@deprecated "Use `List.tail` instead."]
  let tail_opt (type elt) (list: elt t) : elt t option =
    tail list

  (** display-only-for-cameligo
     The call `map f [a1; ...; an]` applies the function `f` to `a1`,
    ..., `an` (from left to right), and builds the list
    `[f a1; ...; f an]` with the results returned by `f`. *)
  (** display-only-for-jsligo
    The call `map(f, list([a1; ...; an]))` applies the function `f` to
    `a1`, ..., `an` (from left to right), and builds the list
    `list([f(a1); ...; f(an)])` with the results returned by `f`. *)
  let map (type src dst) (f: src -> dst) (list: src list) : dst list =
    [%external ("LIST_MAP", f, list)]

  (** display-only-for-cameligo
    The call `iter f [a1; ...; an]` applies the function `f` in turn
    to `[a1; ...; an]`. It is equivalent to
    `let () = f a1 in let () = f a2 in ... in f an`. *)
  (** display-only-for-jsligo
    The call `iter(f, list([a1; ...; an]))` applies the function `f`
    in turn to `list([a1; ...; an])`. It is equivalent to `{f(a1);
    f(a2); ...; f(an)}`. *)
  let iter (type elt) (f: elt -> unit) (list: elt t) : unit =
    [%external ("LIST_ITER", f, list)]

  (** display-only-for-cameligo
    The call `fold_left f init [a1; ...; an]` is
    `f (... (f (f init a1) a2) ...) an`. *)
  (** display-only-for-jsligo
    The call `fold_left(f, init, list([a1; ...; an]))` is
    `f (... (f (f(init, a1)), a2), ...), an)`. *)
  let fold_left
    (type elt acc) (f: acc * elt -> acc) (init: acc) (list: elt t) : acc =
    [%external ("LIST_FOLD_LEFT", f, init, list)]

  (** display-only-for-cameligo
    The call `fold_right f [a1; ...; an] init` is
    `f a1 (f a2 (... (f an init) ...))`. *)
  (** display-only-for-jsligo
    The call `fold_right(f, list([a1; ...; an]), init)` is
    `f (a1, f (a2, (..., f (an, init))...))`. *)
  let fold_right
    (type elt acc) (f: elt * acc -> acc) (list: elt t) (init: acc) : acc =
    [%external ("LIST_FOLD_RIGHT", f, list, init)]

  (** display-only-for-cameligo
    The call `fold f [a1; ...; an] init` is
    `f (... (f (f init a1) a2) ...) an`. Note:
    `fold_left f init list` is the same as `fold f list init`. *)
  (** display-only-for-jsligo
    The call `fold(f, list([a1; ...; an]), init)` is
    `f (... (f (f (init, a1), a2) ...), an)`. Note:
    `fold_left(f, init, list)` is the same as `fold(f, list, init)`. *)
  [@inline]
  let fold
    (type elt acc) (f: acc * elt -> acc) (list: elt t) (init: acc) : acc =
    (* TODO: Remove constant LIST_FOLD. *)
    fold_left f init list

  (** display-only-for-cameligo
    The call `cons e l` is `e :: l`. *)
  (** display-only-for-jsligo
    The call `cons(e, l)` is `list([e, ...l])`. *)
  let cons (type elt) (elt: elt) (list: elt t) : elt t =
    (* TODO: Remove constant CONS. *)
    elt :: list

  (** display-only-for-cameligo
    The call `find_opt pred list` is `None` if no element of the
    list `list` satisfies the predicate `pred`; otherwise, it is
    `Some e`, where `e` is the leftmost element in `list` that satisfies
    `pred`. The order of the calls of `pred` is not specified. *)
  (** display-only-for-jsligo
    The call `find_opt(pred, list)` is `None()` if no element of the
    list `list` satisfies the predicate `pred`; otherwise, it is
    `Some(e)`, where `e` is the leftmost element in `list` that satisfies
    `pred`. The order of the calls of `pred` is not specified. *)
  let find_opt
    (type elt) (pred: elt -> bool) (list: elt t) : elt option =
    let aux (elt, acc : elt * elt option) : elt option =
      if pred elt then Some elt else acc
    in fold_right aux list None

  (** display-only-for-cameligo
    The call `filter_map f l` is the maximal sub-list of `l` such
    that the call of function `f` on its elements is not `None`. Note:
    `f` is called on all elements of `l`. The order of the calls of
    `f` is not specified. *)
  (** display-only-for-jsligo
    The call `filter_map(f, l)` is the maximal sub-list of `l` such
    that the call of function `f` on its elements is not `None()`. Note:
    `f` is called on all elements of `l`. The order of the calls of
    `f` is not specified. *)
  let filter_map
    (type src dst) (filter: src -> dst option) (list: src list) : dst list =
    let f (elt, acc : src * dst list) =
      match filter elt with
      | None -> acc
      | Some dst -> dst :: acc
    in fold_right f list []

  (** display-only-for-cameligo
    The call `update f l` is the list `l` where the elements `e`
    such that `f e` is `Some v` have been replaced by `v`. *)
  (** display-only-for-jsligo
    The call `update(f, l)` is the list `l` where the elements `e`
    such that `f(e)` is `Some(v)` have been replaced by `v`. *)
  let update
    (type elt) (filter: elt -> elt option) (list: elt t) : elt t =
    let f elt =
      match filter elt with
      | None -> elt
      | Some new_elt -> new_elt
    in map f list

  (** display-only-for-cameligo
    The call `update_with p d l` is the list `l` where the elements
    `e` such that satisfy the predicate `p` are replaced by `d`. *)
  (** display-only-for-jsligo
    The call `update_with(p,d,l)` is the list `l` where the elements
    `e` such that satisfy the predicate `p` are replaced by `d`. *)
  let update_with
    (type elt) (pred: elt -> bool) (default: elt) (list: elt t) : elt t =
    map (fun elt -> if pred elt then default else elt) list
end

(** Maps from keys to values, where the bindings key/value are ordered
  by increasing keys. *)
module Map = struct

  (** display-only-for-cameligo
    The type `('key,'value) t` is an alias for `('key,'value) map`. *)
  (** display-only-for-jsligo
    The type `t<key, value>` is an alias for `map<key,value>`. *)
  type ('key,'value) t = ('key,'value) map

  (** display-only-for-cameligo
    The value `empty` is the empty map. In some contexts, it is
    useful to annotate it with its type, for example:
    `(empty : (int, string) map)`. *)
  (** display-only-for-jsligo
    The value `empty` is the empty map. In some contexts, it is
    useful to annotate it with its type, for example:
    `(empty as map<int, string>)`. *)
  let empty (type key value) : (key, value) t =
    [%external "MAP_EMPTY"]

  (** display-only-for-cameligo
    The call `get_and_update key None map` returns a copy of the map
    `map` without the entry for the key `key` in `map` (no change if
    the key is absent). The call `get_and_update key (Some value) map`
    returns a copy of the map `map` where there is an entry for the
    key `key` associated with the value `value`. In both cases, if
    there was already a value `v` bound to `key`, it is returned as
    `Some v`, otherwise `None`. *)
  (** display-only-for-jsligo
    The call `get_and_update(key, None(), map)` returns a copy of the
    map `map` without the entry for the key `key` in `map` (no change
    if the key is absent). The call `get_and_update(key, Some(value),
    map)` returns a copy of the map `map` where there is an entry for
    the key `key` associated with the value `value`. In both cases, if
    there was already a value `v` bound to `key`, it is returned as
    `Some(v)`, otherwise `None()`. *)
  let get_and_update
    (type key value) (key: key) (upd: value option) (map: (key, value) t)
    : value option * (key, value) t =
    [%external ("MAP_GET_AND_UPDATE", key, upd, map)]

  (** display-only-for-cameligo
    The call `update key None map` returns a copy of the map `map`
    without the entry for the key `key` in `map` (no change if the key
    is absent). The call `update key (Some value) map` returns the map
    `map` where there is an entry for the key `key` associated with
    the value `value`. In both cases, the value originally bound to
    `key` is lost. See `get_and_update`. *)
  (** display-only-for-jsligo
    The call `update(key, None(), map)` returns a copy of the map `map`
    without the entry for the key `key` in `map` (no change if the key
    is absent). The call `update(key, Some(value), map)` returns the map
    `map` where there is an entry for the key `key` associated with
    the value `value`. In both cases, the value originally bound to
    `key` is lost. See `get_and_update`. *)
  let update
    (type key value) (key: key) (upd: value option) (map: (key, value) t)
    : (key, value) t =
    (* TODO: Remove constant MAP_UPDATE. *)
    get_and_update key upd map |> snd

  (** display-only-for-cameligo
    The call `add key value map` returns a copy of the `map` where
    there is a binding of key `key` to value `value`. If there is a
    binding for `key` in `map`, then it is lost. *)
  (** display-only-for-jsligo
    The call `add(key, value, map)` returns a copy of the `map` where
    there is a binding of key `key` to value `value`. If there is a
    binding for `key` in `map`, then it is lost. *)
  let add (type key value) (key: key) (value: value) (map: (key, value) t)
    : (key, value) t =
    (* TODO: Remove constant MAP_ADD. *)
    update key (Some value) map

  (** display-only-for-cameligo
    The call `remove key map` returns a copy of the map `map` where
    the binding for key `key` is absent. *)
  (** display-only-for-jsligo
    The call `remove(key, map)` returns a copy of the map `map` where
    the binding for key `key` is absent. *)
  let remove (type key value) (key: key) (map: (key, value) t)
    : (key, value) t =
    (* TODO: Remove constant MAP_REMOVE. *)
    update key None map

  (** display-only-for-cameligo
    The call `literal [(k1,v1); ...; (kn,vn)]` returns a map from
    the pairs of key/value in the list. Note: The list must be a
    literal, not an expression (compile-time list of values). *)
  (** display-only-for-jsligo
    The call `literal(list[[k1,v1], ..., [kn,vn]])` returns a map from
    the pairs of key/value in the list. Note: The list must be a
    literal, not an expression (compile-time list of values). *)
  [@thunk] [@inline]
  let literal (type key value) (bindings: (key * value) list)
    : (key, value) t =
    [%external ("MAP_LITERAL", bindings)]

  (** display-only-for-cameligo
    The call `of_list bindings` returns a map from the pairs of
    key/value in the list `bindings`. Note: Use `literal` instead if
    using a literal list. *)
  (** display-only-for-jsligo
    The call `of_list(bindings)` returns a map from the pairs of
    key/value in the list `bindings`. Note: Use `literal` instead if
    using a literal list. *)
  let of_list (type key value) (bindings: (key * value) list)
    : (key, value) t =
    let update (map, (key, value)) = add key value map
    in List.fold_left update empty bindings

  (** display-only-for-cameligo
    The call `size map` evaluates in the number of entries in the
    map `map`. *)
  (** display-only-for-jsligo
    The call `size(map)` evaluates in the number of entries in the
    map `map`. *)
  let size (type key value) (map: (key, value) t) : nat =
    [%external ("MAP_SIZE", map)]

  (** display-only-for-cameligo
    The call `mem key map` is `true` if, and only if, the key `key`
    is in the map `map`. *)
  (** display-only-for-jsligo
    The call `mem(key, map)` is `true` if, and only if, the key `key`
    is in the map `map`. *)
  let mem (type key value) (key: key) (map: (key, value) t) : bool =
    [%external ("MAP_MEM", key, map)]

  (** display-only-for-cameligo
    The call `find_opt key map` returns `None` if the key `key` is
    present in the map `map`; otherwise, it is `Some v`, where `v` is
    the value associated to `key` in `map`. *)
  (** display-only-for-jsligo
    The call `find_opt(key, map)` returns `None()` if the key `key` is
    present in the map `map`; otherwise, it is `Some(v)`, where `v` is
    the value associated to `key` in `map`. *)
  let find_opt (type key value) (key: key) (map: (key, value) t)
    : value option =
    [%external ("MAP_FIND_OPT", key, map)]

  (** display-only-for-cameligo
    The call `find key map` returns the value associated to `key` in
    `map`. If the key is absent, the execution fails with the string
    `"MAP FIND"`. *)
  (** display-only-for-jsligo
    The call `find(key, map)` returns the value associated to `key` in
    `map`. If the key is absent, the execution fails with the string
    `"MAP FIND"`. *)
  let find (type key value) (key: key) (map: (key, value) t) : value =
    (* TODO: Remove constant MAP_FIND. *)
    match find_opt key map with
      None -> failwith "MAP FIND"
    | Some value -> value

  (** display-only-for-cameligo
    The call `fold f map init` is
    `f ( ... f (f (init, (k1,v1)), (k2,v2)), ..., (kn,vn))`
    where `(k1,v1)`, `(k2,v2)`, ..., `(kn,vn)` are the bindings in the
    map `map`, in increasing order of the keys `k1`, `k2`, ..., and `kn`. *)
  (** display-only-for-jsligo
    The call `fold(f, map, init)` is
    `f (... f (f (init, [k1,v1]), [k2,v2]), ..., [kn,vn])`
    where `[k1,v1]`, `[k2,v2]`, ..., `[kn,vn]` are the bindings in the
    map `map`, in increasing order of the keys `k1`, `k2`, ..., and `kn`. *)
  let fold
    (type key value acc)
    (f: acc * (key * value) -> acc)
    (map: (key, value) t)
    (init: acc)
    : acc =
    [%external ("MAP_FOLD", f, map, init)]

  (** display-only-for-cameligo
    The call `iter f map` is
    `let () = f (k1,v1) in let () = f (k2,v2) in ... in f (kn,vn)`. *)
  (** display-only-for-jsligo
    The call `iter(f, map)` is `{f (k1,v1); (k2,v2); ...; f (kn,vn);}`. *)
  let iter
    (type key value) (f: key * value -> unit) (map: (key, value) t) : unit =
    [%external ("MAP_ITER", f, map)]
    (* The following code generates a LAMBDA instruction, with
       restrictions on what can be captured in it:

       fold (fun ((), binding : unit * (key * value)) -> f binding) map ()
    *)

  (** display-only-for-cameligo
    The call `map f m`, where the map `m` contains the bindings
    `(k1,v1)`, `(k2,v2)`, ..., and `(kn,vn)` in increasing order of
    the keys, is the map containing the bindings `(k1, f (k1,v1))`,
    `(k2, f (k2,v2))`, ..., `(kn, f (kn,vn))`. *)
  (** display-only-for-jsligo
    The call `map(f, m)`, where the map `m` contains the bindings
    `[k1,v1]`, `[k2,v2]`, ..., and `[kn,vn]` in increasing order of
    the keys, is the map containing the bindings `[k1, f (k1,v1)]`,
    `[k2, f (k2,v2)]`, ..., `[kn, f (kn,vn)]`. *)
  let map
    (type key value new_value)
    (f: key * value -> new_value)
    (map: (key, value) t)
    : (key, new_value) t =
    [%external ("MAP_MAP", f, map)]
    (* The following code generates a LAMBDA instruction, with
       restrictions on what can be captured in it:

       let f (new_map, (key, value)) = add key (f (key, value)) new_map
       in fold f map empty *)
end

(** Maps from keys to values, lazily accessed and where the bindings
  key/value are ordered by increasing keys. *)
module Big_map = struct

  (** display-only-for-cameligo
    The type `('key,'value) t` is an alias for
    `('key,'value) big_map`. *)
  (** display-only-for-jsligo
    The type `t<key, value>` is an alias for `big_map<key, value>`. *)
  type ('key,'value) t = ('key,'value) big_map

  (** display-only-for-cameligo
    The value `empty` is the empty big map. In some contexts, it is
    useful to annotate it with its type, for example:
    `(empty : (int, string) big_map)`.*)
  (** display-only-for-jsligo
    The value `empty` is the empty big map. In some contexts, it is
    useful to annotate it with its type, for example:
    `(empty as big_map<int, string>`.*)
  [@inline]
  let empty (type key value) : (key, value) t =
    [%external "BIG_MAP_EMPTY"]

  (** display-only-for-cameligo
    The call `get_and_update key None map` returns a copy of the map
    `map` without the entry for the key `key` in `map` (no change if
    the key is absent). The call `get_and_update key (Some value) map`
    returns a copy of the map `map` where there is an entry for the
    key `key` associated with the value `value`. In both cases, if
    there was already a value `v` bound to `key`, it is returned as
    `Some v`, otherwise `None`. *)
  (** display-only-for-jsligo
    The call `get_and_update(key, None(), map)` returns a copy of the map
    `map` without the entry for the key `key` in `map` (no change if
    the key is absent). The call `get_and_update(key, Some(value), map)`
    returns a copy of the map `map` where there is an entry for the
    key `key` associated with the value `value`. In both cases, if
    there was already a value `v` bound to `key`, it is returned as
    `Some(v)`, otherwise `None()`. *)
  let get_and_update
    (type key value) (key: key) (upd: value option) (map: (key, value) t)
    : value option * (key, value) t =
    [%external ("BIG_MAP_GET_AND_UPDATE", key, upd, map)]

  (** display-only-for-cameligo
    The call `update key None map` returns a copy of the map `map`
    without the entry for the key `key` in `map` (no change if the key
    is absent). The call `update key (Some value) map` returns the map
    `map` where there is an entry for the key `key` associated with
    the value `value`. In both cases, the value originally bound to
    `key` is lost. See `get_and_update`. *)
  (** display-only-for-jsligo
    The call `update(key, None(), map)` returns a copy of the map `map`
    without the entry for the key `key` in `map` (no change if the key
    is absent). The call `update(key, Some(value), map)` returns the map
    `map` where there is an entry for the key `key` associated with
    the value `value`. In both cases, the value originally bound to
    `key` is lost. See `get_and_update`. *)
  let update
    (type key value) (key: key) (upd: value option) (map: (key, value) t)
    : (key, value) t =
    (* TODO: Remove constant MAP_UPDATE. *)
    get_and_update key upd map |> snd

  (** display-only-for-cameligo
    The call `add key value map` returns a copy of the `map` where
    there is a binding of key `key` to value `value`. If there is a
    binding for `key` in `map`, then it is lost. *)
  (** display-only-for-jsligo
    The call `add(key, value, map)` returns a copy of the `map` where
    there is a binding of key `key` to value `value`. If there is a
    binding for `key` in `map`, then it is lost. *)
  let add
    (type key value) (key: key) (value: value) (map: (key, value) t)
    : (key, value) t =
    (* TODO: Remove constant MAP_ADD. *)
    update key (Some value) map

  (** display-only-for-cameligo
    The call `remove key map` returns a copy of the map `map` where
    the binding for key `key` is absent. *)
  (** display-only-for-jsligo
    The call `remove(key, map)` returns a copy of the map `map` where
    the binding for key `key` is absent. *)
  let remove (type key value) (key: key) (map: (key, value) t)
    : (key, value) t =
    (* TODO: Remove constant MAP_REMOVE. *)
    update key None map

  (** display-only-for-cameligo
    The call `literal [(k1,v1); ...; (kn,vn)]` returns a big map from
    the pairs of key/value in the list. Note: The list must be a
    literal, not an expression (compile-time list of values). *)
  (** display-only-for-jsligo
    The call `literal(list[[k1,v1], ..., [kn,vn]])` returns a big map
    from the pairs of key/value in the list. Note: The list must be a
    literal, not an expression (compile-time list of values). *)
  [@thunk] [@inline]
  let literal (type key value) (bindings: (key * value) list)
    : (key, value) t =
    [%external ("BIG_MAP_LITERAL", bindings)]

  (** display-only-for-cameligo
    The call `of_list bindings` returns a big map from the pairs of
    key/value in the list `bindings`. Note: Use `literal` instead if
    using a literal list. *)
  (** display-only-for-jsligo
    The call `of_list(bindings)` returns a big map from the pairs of
    key/value in the list `bindings`. Note: Use `literal` instead if
    using a literal list. *)
  let of_list (type key value) (bindings: (key * value) list)
    : (key, value) t =
    let update (map, (key, value)) = add key value map
    in List.fold_left update empty bindings

  (** display-only-for-cameligo
    The call `mem key map` is `true` if, and only if, the key `key`
    is in the big map `map`. *)
  (** display-only-for-jsligo
    The call `mem(key, map)` is `true` if, and only if, the key `key`
    is in the big map `map`. *)
  let mem (type key value) (key: key) (map: (key, value) t) : bool =
    [%external ("MAP_MEM", key, map)]

  (** display-only-for-cameligo
    The call `find_opt key map` returns `None` if the key `key` is
    present in the big map `map`; otherwise, it is `Some v`, where `v`
    is the value associated to `key` in `map`. *)
  (** display-only-for-jsligo
    The call `find_opt(key, map)` returns `None()` if the key `key` is
    present in the big map `map`; otherwise, it is `Some(v)`, where `v`
    is the value associated to `key` in `map`. *)
  let find_opt (type key value) (key: key) (map: (key, value) t)
    : value option =
    [%external ("MAP_FIND_OPT", key, map)]

  (** display-only-for-cameligo
    The call `find key map` returns the value associated to `key` in
    `map`. If the key is absent, the execution fails with the string
    `"MAP FIND"`. *)
  (** display-only-for-jsligo
    The call `find(key, map)` returns the value associated to `key` in
    `map`. If the key is absent, the execution fails with the string
    `"MAP FIND"`. *)
  let find (type key value) (key: key) (map: (key, value) t) : value =
    (* TODO: Remove constant MAP_FIND. *)
    match find_opt key map with
      None -> failwith "MAP FIND"
    | Some value -> value
end

(** Totally ordered sets *)
module Set = struct

  (** display-only-for-cameligo
    The type `'elt t` is an alias for `'elt set`. *)
  (** display-only-for-jsligo
    The type `t<elt>` is an alias for `set<elt>`. *)
  type 'elt t = 'elt set

  (** display-only-for-jsligo
    The value `empty` denotes the empty set. In some contexts, it is
    useful to annotate it with its type, for example:
    `(empty as set<int>)`. *)
  [@inline]
  let empty (type elt) : elt t =
    [%external "SET_EMPTY"]

  (** display-only-for-cameligo
    The call `update elt true set` is a copy of the set `set`
    containing the element `elt`. The call `update elt false set` is a
    copy of the set `set` where the element `elt` is absent. *)
  (** display-only-for-jsligo
    The call `update(elt, true, set)` is a copy of the set `set`
    containing the element `elt`. The call `update(elt, false, set)` is a
    copy of the set `set` where the element `elt` is absent. *)
  let update (type elt) (elt: elt) (add: bool) (set: elt t) : elt t =
    [%external ("SET_UPDATE", elt, add, set)]

  (** display-only-for-cameligo
    The call `add elt set` is a set containing all the elements of
    the set `set`, plus the element `elt`. *)
  (** display-only-for-jsligo
    The call `add(elt, set)` is a set containing all the elements of
    the set `set`, plus the element `elt`. *)
  let add (type elt) (elt: elt) (set: elt t) : elt t =
    (* TODO: Remove constant SET_ADD. *)
    update elt true set

  (** display-only-for-cameligo
    The call `remove elt set` is a copy of the set `set` without the
    element `elt`. *)
  (** display-only-for-jsligo
    The call `remove(elt, set)` is a copy of the set `set` without the
    element `elt`. *)
  let remove (type elt) (elt: elt) (set: elt t) : elt t =
    (* TODO: Remove constant SET_REMOVE. *)
    update elt false set

  (** display-only-for-cameligo
    The call `literal [e1; ...; en]` is a set containing exactly the
    elements in the list. Note: The list must be literal, not an
    expression (compile-time list of values). *)
  (** display-only-for-jsligo
    The call `literal(list([e1, ..., en]))` is a set containing
    exactly the elements in the list. Note: The list must be literal,
    not an expression (compile-time list of values). *)
  [@thunk] [@inline]
  let literal (type elt) (list: elt list) : elt t =
    [%external ("SET_LITERAL", list)]

  (** display-only-for-cameligo
    The call `of_list elements` is a set containing exactly the
    elements in the list `elements`. Note: Use `literal` instead if
    using a literal list. Note: Use `literal` instead if using a
    literal list. *)
  (** display-only-for-jsligo
    The call `of_list(elements)` is a set containing exactly the
    elements in the list `elements`. Note: Use `literal` instead if
    using a literal list. Note: Use `literal` instead if using a
    literal list. *)
  let of_list (type elt) (elements: elt list) : elt t =
    List.fold_left (fun (set, elt) -> add elt set) empty elements

  (** display-only-for-cameligo
    The call `size set` is the number of elements of the set `set`. *)
  (** display-only-for-jsligo
    The call `size(set)` is the number of elements of the set `set`. *)
  let size (type elt) (set: elt t) : nat =
    [%external ("SET_SIZE", set)]

  (** display-only-for-cameligo
    The call `cardinal set` is the number of elements of the set `set`. *)
  (** display-only-for-jsligo
    The call `cardinal(set)` is the number of elements of the set `set`. *)
  [@inline]
  let cardinal (type elt) (set: elt t) : nat = size set

  (** display-only-for-cameligo
    The call `mem elt set` is `true` if, and only if, the element
    `elt` belongs to the set `set`. *)
  (** display-only-for-jsligo
    The call `mem(elt, set)` is `true` if, and only if, the element
    `elt` belongs to the set `set`. *)
  let mem (type elt) (elt: elt) (set: elt t) : bool =
    [%external ("SET_MEM", elt, set)]

  (** display-only-for-cameligo
    The call `fold f set init` is
    `f(... (f (f (init, e1), e2), ...), en)`,
    where `e1`, `e2`, ..., `en` are the elements of the set `set` in
    increasing order. *)
  (** display-only-for-jsligo
    The call `fold(f, set, init)` is
    `f(... (f (f (init, e1), e2), ...), en)`,
    where `e1`, `e2`, ..., `en` are the elements of the set `set` in
    increasing order. *)
  let fold
    (type elt acc) (f: acc * elt -> acc) (set: elt t) (init: acc) : acc =
    [%external ("SET_FOLD", f, set, init)]

  (** display-only-for-cameligo
    The call `fold f set init` is `f(... (f (init, en), ...), e1)`,
    where `e1`, `e2`, ..., `en` are the elements of the set `set` in
    increasing order. *)
  (** display-only-for-jsligo
    The call `fold(f, set, init)` is `f(... (f (init, en), ...), e1)`,
    where `e1`, `e2`, ..., `en` are the elements of the set `set` in
    increasing order. *)
  let fold_desc
    (type elt acc) (f: elt * acc -> acc) (set: elt t) (init: acc) : acc =
    [%external ("SET_FOLD_DESC", f, set, init)]

  (** display-only-for-cameligo
    The call `filter_map f set` is a set made by calling `f` (the
    filter) on each element of the set `set`: if `f` returns `None`,
    the element is skipped in the result, otherwise, if it is
    `Some e`, then `e` is kept. *)
  (** display-only-for-jsligo
    The call `filter_map(f, set)` is a set made by calling `f` (the
    filter) on each element of the set `set`: if `f` returns `None()`,
    the element is skipped in the result, otherwise, if it is
    `Some(e)`, then `e` is kept. *)
  let filter_map
    (type old new) (filter: old -> new option) (set: old t) : new t =
    let f (old, set) =
      match filter old with
        None -> set
      | Some new -> add new set
    in fold_desc f set empty

  (** display-only-for-cameligo
    The call `iter f set` applies `f` to all the elements of the set
    `set` in increasing order. *)
  (** display-only-for-jsligo
    The call `iter(f, set)` applies `f` to all the elements of the set
    `set` in increasing order. *)
  let iter (type elt) (f: elt -> unit) (set: elt t) : unit =
    (* TODO: Remove constant SET_ITER. *)
    fold (fun ((), elt : unit * elt) -> f elt) set ()

  (** display-only-for-cameligo
    The call `map f set` evaluates in a set whose elements have been
    obtained by applying `f` to the elements of the set `set`. *)
  (** display-only-for-jsligo
    The call `map(f, set)` evaluates in a set whose elements have been
    obtained by applying `f` to the elements of the set `set`. *)
  let map
    (type old new) (f: old -> new) (set: old t) : new t =
    fold (fun (set, elt) -> add (f elt) set) set empty
end

(** Lazily accessed sets *)
module Big_set = struct

  (** display-only-for-cameligo
    The type of the big sets is based on `big_map`. *)
  (** display-only-for-jsligo
    The type of the big sets is based on `big_map`. *)
  type 'elt t = ('elt, unit) big_map

  (** display-only-for-cameligo
    The value `empty` denotes the empty big set. In some contexts,
    it is useful to annotate it with its type, for example:
    `(empty as Big_set.t<int>)`. *)
  let empty (type elt) : elt t = Big_map.empty

  (** display-only-for-cameligo
    The call `update elt true set` is a copy of the big set `set`
    containing the element `elt`. The call `update elt false set` is a
    copy of the big set `set` where the element `elt` is absent. *)
  (** display-only-for-jsligo
    The call `update(elt, true, set)` is a copy of the big set `set`
    containing the element `elt`. The call `update(elt, false, set)`
    is a copy of the big set `set` where the element `elt` is
    absent. *)
  let update (type elt) (elt: elt) (add: bool) (set: elt t) : elt t =
    Big_map.update elt (if add then Some () else None) set

  (** display-only-for-cameligo
    The call `add elt set` is a big set containing all the elements
    of the big set `set`, plus the element `elt`. *)
  (** display-only-for-jsligo
    The call `add(elt, set)` is a big set containing all the elements
    of the big set `set`, plus the element `elt`. *)
  let add (type elt) (elt: elt) (set: elt t) : elt t =
    Big_map.add elt () set

  (** display-only-for-cameligo
    The call `remove elt set` is a copy of the set `set` without the
    element `elt`. *)
  (** display-only-for-jsligo
    The call `remove(elt, set)` is a copy of the set `set` without the
    element `elt`. *)
  let remove (type elt) (elt: elt) (set: elt t) : elt t =
    Big_map.remove elt set

  (** display-only-for-cameligo
    The call `literal [e1; ...; en]` is a big set containing exactly
    the elements in the list. Note: The list must be literal, not an
    expression (compile-time list of values). *)
  (** display-only-for-jsligo
    The call `literal(list([e1, ..., en]))` is a big set containing
    exactly the elements in the list. Note: The list must be literal,
    not an expression (compile-time list of values). *)
  [@thunk] [@inline]
  let literal (type elt) (list: elt list) : elt t =
    [%external ("BIG_SET_LITERAL", list)]

  (** display-only-for-cameligo
    The call `of_list elements` is a big set containing exactly the
    elements in the list `elements`. Note: Use `literal` instead if
    using a literal list. Note: Use `literal` instead if using a
    literal list. *)
  (** display-only-for-jsligo
    The call `of_list(elements)` is a big set containing exactly the
    elements in the list `elements`. Note: Use `literal` instead if
    using a literal list. Note: Use `literal` instead if using a
    literal list. *)
  let of_list (type elt) (elements : elt list) : elt t =
    List.fold_left (fun (set, elt) -> add elt set) empty elements

  (** display-only-for-cameligo
    The call `mem elt set` is `true` if, and only if, the element
    `elt` belongs to the big set `set`. *)
  (** display-only-for-jsligo
    The call `mem(elt, set)` is `true` if, and only if, the element
    `elt` belongs to the big set `set`. *)
  let mem (type elt) (elt: elt) (set: elt t) : bool =
    Big_map.mem elt set
end

(** display-only-for-cameligo
  The type of the big sets is based on `big_map`. *)
(** display-only-for-jsligo
  The type of the big sets is based on `big_map`. *)
type 'elt big_set = 'elt Big_set.t

(** Strings of characters *)
module String = struct

  (** display-only-for-cameligo
      The call `length s` is the number of characters in the string
      `s`. Note: `String.length` is another name for `String.size`. *)
  (** display-only-for-jsligo
      The call `length(s)` is the number of characters in the string
      `s`. Note: `String.length` is another name for `String.size`. *)
  let length (string: string) : nat =
    [%external ("SIZE", string)]

  (** display-only-for-cameligo
    The call `size s` is the number of characters in the string `s`. *)
  (** display-only-for-jsligo
    The call `size(s)` is the number of characters in the string `s`. *)
  [@inline]
  let size (string: string) : nat = length string

  (** display-only-for-cameligo
    The call `concat left right` is the concatenation of the string
    `left` and the string `right`, in that order.  *)
  (** display-only-for-jsligo
    The call `concat(left, right)` is the concatenation of the string
    `left` and the string `right`, in that order.  *)
  let concat (left: string) (right: string) : string =
    (* TODO: Remove constant CONCAT. *)
    left ^ right

  (** display-only-for-cameligo
    The call `concats list` is the concatenation of the strings in
    the list `list`, from left to right. *)
  (** display-only-for-jsligo
    The call `concats(list)` is the concatenation of the strings in
    the list `list`, from left to right. *)
  let concats (list: string list) : string =
    (* TODO: Remove constant CONCATS. *)
    List.fold_right (uncurry concat) list ""

  (** display-only-for-cameligo
    The call `sub index len str` is the substring of string `str`
    starting at index `index` (0 denoting the first character) and of
    length `len`. If the index or length are invalid, an exception
    interrupts the execution. *)
  (** display-only-for-jsligo
    The call `sub(index, len, str)` is the substring of string `str`
    starting at index `index` (0 denoting the first character) and of
    length `len`. If the index or length are invalid, an exception
    interrupts the execution. *)
  let sub (index: nat) (length: nat) (string: string) : string =
    [%external ("SLICE", index, length, string)]

  (** display-only-for-cameligo
    The call `slice index len str` is the substring of string `str`
    starting at index `index` (0 denoting the first character) and of
    length `len`. If the index or length are invalid, an exception
    interrupts the execution. *)
  (** display-only-for-jsligo
    The call `slice(index, len, str)` is the substring of string `str`
    starting at index `index` (0 denoting the first character) and of
    length `len`. If the index or length are invalid, an exception
    interrupts the execution. *)
  [@thunk] [@inline]
  let slice (index: nat) (length: nat) (string: string) : string =
    sub index length string
end

(** Sequences of bytes

    Bytes are used for serializing data, in order to check signatures
    and compute hashes on them. They can also be used to read untyped
    data from outside of the contract. *)
module Bytes = struct

  (** display-only-for-cameligo
      The call `length b` is the number of bytes in the sequence of
      bytes `b`. Note: `Bytes.length` is another name for
      `Bytes.size`. *)
  (** display-only-for-jsligo
      The call `length(b)` is the number of bytes in the sequence of
      bytes `b`. Note: `Bytes.length` is another name for
      `Bytes.size`. *)
  let length (bytes: bytes) : nat =
    [%external ("SIZE", bytes)]

  (** display-only-for-cameligo
    The call `size b` is the number of bytes in the sequence of
    bytes `b`.  *)
  (** display-only-for-jsligo
    The call `size(b)` is the number of bytes in the sequence of
    bytes `b`.  *)
  [@thunk] [@inline]
  let size (bytes: bytes) : nat = length bytes

  (** display-only-for-cameligo
    The call `concat left right` is the sequence of bytes obtained
    by concatenating the sequence `left` before the sequence
    `right`. *)
  (** display-only-for-jsligo
    The call `concat(left, right)` is the sequence of bytes obtained
    by concatenating the sequence `left` before the sequence
    `right`. *)
  let concat (left: bytes) (right: bytes) : bytes =
    [%external ("CONCAT", left, right)]

  (** display-only-for-cameligo
    The call `concats list` is the concatenation of the byte
    sequences in the list `list`, from left to right. *)
  (** display-only-for-jsligo
    The call `concats(list)` is the concatenation of the byte
    sequences in the list `list`, from left to right. *)
  let concats (list: bytes list) : bytes =
    (* TODO: Remove constant CONCATS. *)
    List.fold_right (uncurry concat) list 0x

  (** display-only-for-cameligo
    The call `sub index len bytes` is the subsequence of bytes
    `bytes` starting at index `index` (0 denoting the first byte) and
    of length `len`. If the index or length are invalid, an exception
    interrupts the execution. *)
  (** display-only-for-jsligo
    The call `sub(index, len, bytes)` is the subsequence of bytes
    `bytes` starting at index `index` (0 denoting the first byte) and
    of length `len`. If the index or length are invalid, an exception
    interrupts the execution. *)
  let sub (index: nat) (length: nat) (bytes: bytes) : bytes =
    [%external ("SLICE", index, length, bytes)]

  (** display-only-for-cameligo
    The call `slice index len bytes` is the subsequence of bytes
    `bytes` starting at index `index` (0 denoting the first byte) and
    of length `len`. If the index or length are invalid, an exception
    interrupts the execution. *)
  (** display-only-for-jsligo
    The call `slice(index, len, bytes)` is the subsequence of bytes
    `bytes` starting at index `index` (0 denoting the first byte) and
    of length `len`. If the index or length are invalid, an exception
    interrupts the execution. *)
  [@thunk] [@inline]
  let slice (index: nat) (length: nat) (bytes: bytes) : bytes =
    sub index length bytes

  (** display-only-for-cameligo
    The call `pack v` transforms the value `v` into a sequence of
    bytes. *)
  (** display-only-for-jsligo
    The call `pack(v)` transforms the value `v` into a sequence of
    bytes. *)
  let pack (type a) (value: a) : bytes =
    [%michelson ({| {PACK} |} value : bytes)]

  (** display-only-for-cameligo
    The call `unpack bytes` is `Some v` if the sequence of bytes
    `bytes` decodes into a valid LIGO value `v`; otherwise `None`. *)
  (** display-only-for-jsligo
    The call `unpack(bytes)` is `Some(v)` if the sequence of bytes
    `bytes` decodes into a valid LIGO value `v`; otherwise
    `None()`. *)
  let unpack (type a) (bytes: bytes) : a option =
    [%michelson ({| {UNPACK (typeopt $0)} |} (None : a option) bytes
                 : a option)]
end

(** Cryptographic primitives *)
module Crypto = struct

  (** Compute the cryptographic hash of the top of the stack using the
    Blake2b-256 cryptographic hash function. *)
  let blake2b (bytes: bytes) : bytes =
    [%michelson ({| {BLAKE2B} |} bytes : bytes)]

  (** Compute the cryptographic hash of the top of the stack using the
    SHA-256 cryptographic hash function. *)
  let sha256 (bytes: bytes) : bytes =
    [%michelson ({| {SHA256} |} bytes : bytes)]

  (** Compute the cryptographic hash of the top of the stack using the
    SHA-512 cryptographic hash function. *)
  let sha512 (bytes: bytes) : bytes =
    [%michelson ({| {SHA512} |} bytes : bytes)]

  (** Compute the cryptographic hash of the top of the stack using the
    SHA3-256 cryptographic hash function. *)
  let sha3 (bytes: bytes) : bytes =
    [%michelson ({| {SHA3} |} bytes : bytes)]

  (** Compute the cryptographic hash of the top of the stack using the
    Keccak-256 cryptographic hash function. *)
  let keccak (bytes: bytes) : bytes =
    [%michelson ({| {KECCAK} |} bytes : bytes)]

  (** display-only-for-cameligo
    The call `hash_key k` computes the Base58Check of the public key
    `k`. *)
  (** display-only-for-jsligo
    The call `hash_key(k)` computes the Base58Check of the public key
    `k`. *)
  let hash_key (key: key) : key_hash =
    [%michelson ({| {HASH_KEY} |} key : key_hash)]

  (** display-only-for-cameligo
    The call `check k s b` verifies that the byte sequence `b` has
    been signed with the key `k`: it is `true` if, and only if, the
    signature `s` is a valid signature of the byte sequence created
    with `k`. *)
  (** display-only-for-jsligo
    The call `check(k, s, b)` verifies that the byte sequence `b` has
    been signed with the key `k`: it is `true` if, and only if, the
    signature `s` is a valid signature of the byte sequence created
    with `k`. *)
  let check (key: key) (@sig: signature) (bytes: bytes) : bool =
    [%michelson ({| {CHECK_SIGNATURE} |} key @sig bytes : bool)]
end

(** Dynamic entrypoints

   Dynamic entrypoints are lazy entrypoints stored in the contract within
   a big_map. They can then be updated or removed without deploying a new
   contract.

   A contract with dynamic entrypoints must have at least one `@entry`
   declaration (as any other contract); it also must obey some
   convention on storage type definition and have at least one
   `@dyn_entry` declaration.

   LIGO will then include the defined dynamic entries into the
   contract initial storage. *)
type dynamic_entrypoints = (nat, bytes) big_map

(** A value of type `dynamic_entrypoint` denotes a typed key in an
  dynamic entrypoint *)
type dynamic_entrypoint = "%constant:dynamic_entrypoint"

module Dynamic_entrypoints = struct

  (** Type `t` is an alias of the predefined type
      `dynamic_entrypoints`. *)
  type t = dynamic_entrypoints

  [@private]
  let cast_dynamic_entrypoint
    (type param storage) (dyn: (param, storage) dynamic_entrypoint) : nat =
    [%external ("CAST_DYNAMIC_ENTRYPOINT", dyn)]

  (** display-only-for-cameligo
      The call `set dyn None dyn_map` returns a copy of the map of
      dynamic entrypoints `dyn_map` where the dynamic entrypoint `dyn`
      is not associated to a static entrypoint. The call `set dyn
      (Some entrypoint) dyn_map` is a copy of `dyn_map` where the
      dynamic entrypoint `dyn` is associated to the static entrypoint
      `entrypoint`. *)
  (** display-only-for-jsligo
      The call `set(dyn, None(), dyn_map)` returns a copy of the map of
      dynamic entrypoints `dyn_map` where the dynamic entrypoint `dyn`
      is not associated to a static entrypoint. The call `set(dyn,
      Some(entrypoint), dyn_map)` is a copy of `dyn_map` where the
      dynamic entrypoint `dyn` is associated to the static entrypoint
      `entrypoint`. *)
  let set
    (type param storage)
    (dyn: (param, storage) dynamic_entrypoint)
    (entry_opt: (param, storage) entrypoint option)
    (dyn_map: dynamic_entrypoints)
    : dynamic_entrypoints =
    let packed_entry_opt = Option.map Bytes.pack entry_opt in
    Big_map.update (cast_dynamic_entrypoint dyn) packed_entry_opt dyn_map

  (** display-only-for-cameligo
      The call `set_bytes dyn None dyn_map` returns a copy of the map
      of dynamic entrypoints `dyn_map` where the dynamic entrypoint
      `dyn` is not associated to a static entrypoint. The call
      `set_bytes dyn (Some bytes) dyn_map` is a copy of `dyn_map`
      where the dynamic entrypoint `dyn` is associated to the static
      entrypoint encoded by the sequence of bytes `bytes`. If that
      sequence is invalid, any call to the dynamic entrypoint will
      fail. *)
  (** display-only-for-jsligo
      The call `set_bytes(dyn, None(), dyn_map)` returns a copy of the
      map of dynamic entrypoints `dyn_map` where the dynamic
      entrypoint `dyn` is not associated to a static entrypoint. The
      call `set_bytes(dyn, Some(bytes), dyn_map)` is a copy of `dyn_map`
      where the dynamic entrypoint `dyn` is associated to the static
      entrypoint encoded by the sequence of bytes `bytes`. If that
      sequence is invalid, any call to the dynamic entrypoint will
      fail. *)
  let set_bytes
    (type param storage)
    (dyn: (param, storage) dynamic_entrypoint)
    (bytes_opt: bytes option)
    (dyn_map: dynamic_entrypoints)
    : dynamic_entrypoints =
    Big_map.update (cast_dynamic_entrypoint dyn) bytes_opt dyn_map

  (** display-only-for-cameligo
      The call `get dyn dyn_map` is `None` if the dynamic entrypoint
      `dyn` is absent from the dynamic entrypoints map
      `dyn_map`. Otherwise, it is `Some entry`, where `entry` is a
      static entrypoint that is callable (like a function). See type
      `entrypoint`. *)
  (** display-only-for-jsligo
      The call `get(dyn, dyn_map)` is `None()` if the dynamic
      entrypoint `dyn` is absent from the dynamic entrypoints map
      `dyn_map`. Otherwise, it is `Some(entry)`, where `entry` is a
      static entrypoint that is callable (like a function). See type
      `entrypoint`. *)
  let get
    (type param storage)
    (dyn: (param, storage) dynamic_entrypoint)
    (dyn_map: dynamic_entrypoints)
    : (param, storage) entrypoint option =
    let f dyn_fun = Option.value_with_error () (Bytes.unpack dyn_fun) in
    let res_opt = Big_map.find_opt (cast_dynamic_entrypoint dyn) dyn_map
    in Option.map f res_opt

end

(* Michelson constants *)

type michelson_program = "%constant:michelson_program"
type typed_address = "%constant:typed_address"
type mutation = "%constant:mutation"
type michelson_contract = "%constant:michelson_contract"
type pbt_gen = "%constant:pbt_gen"
type int64 = "%constant:int64"
type views = "%constant:views"

type test_exec_error_balance_too_low = {
  contract_balance : tez;
  contract_too_low : address;
  spend_request : tez
}

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

type 'a pbt_test = 'a pbt_gen * ('a -> bool)
type 'a pbt_result = Fail of 'a | Success

type 's unforged_ticket = [@layout:comb] {
  ticketer : address;
  value : 's;
  amount : nat
}

type ('param, 'storage) module_contract =
  ('param * 'storage -> operation list * 'storage)
  * 'storage views * dynamic_entrypoints option

type ('param, 'storage) origination_result = {
  addr : ('param, 'storage) typed_address;
  code : ('param, 'storage) michelson_contract;
  size : int
}

type implicit_address = (unit, unit) typed_address

[@private]
module Toplevel = struct
  module String = String
end

(** The testing framework *)
module Test = struct

  (** Run a function on an input, all in Michelson. More concretely:
    a) compiles the function argument to Michelson `f_mich`; b)
    compiles the value argument (which was evaluated already) to
    Michelson `v_mich`; c) runs the Michelson interpreter on the code
    `f_mich` with starting stack `[v_mich]`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration."]
  let run (type a b) (f : a -> b) (v : a) : michelson_program = [%external ("TEST_RUN", f, v)]

  (** Compile a LIGO value to Michelson. Currently it is a renaming of
    `compile_value`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration."]
  let eval (type a) (x : a) : michelson_program = run (fun (x : a) -> x) x

  (** Decompile a Michelson value to LIGO, following the (mandatory)
  type annotation. Note: This operation can fail at run-time, in case
  that the `michelson_program` given cannot be decompiled to something
  compatible with the annotated type. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration."]
  let decompile (type a) (m : michelson_program) : a = [%external ("TEST_DECOMPILE", m)]

  (** Compile a LIGO value to Michelson. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration."]
  let compile_value (type a) (x : a) : michelson_program = eval x

  (** Returns the total voting power of all contracts. The total
    voting power coincides with the sum of the rolls count of every
    contract in the voting listings. The voting listings is calculated
    at the beginning of every voting period. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_total_voting_power` from `Test.Next` is encouraged for a smoother migration."]
  let get_total_voting_power (_u : unit) : nat = [%external ("TEST_GET_TOTAL_VOTING_POWER", ())]

  (** Cause the testing framework to fail. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.failwith` from `Test.Next` is encouraged for a smoother migration."]
  let failwith (type a b) (v : a) : b = [%external ("TEST_FAILWITH", v)]

  (** Gets the contract corresponding to the default entrypoint of a
    typed address: the contract parameter in the result will be the
    type of the default entrypoint (generally `'param`, but this might
    differ if `'param` includes a "default" entrypoint). *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration."]
  let to_contract (type p s) (t : (p, s) typed_address) : p contract = [%external ("TEST_TO_CONTRACT", t)]

  (** Sets the source for `Test.transfer` and `Test.originate`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration."]
  let set_source (a : address) : unit = [%external ("TEST_SET_SOURCE", a)]

  (** This function casts an address to a typed address. You will need
    to annotate the result with the type you expect. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Address.to_typed_address` from `Test.Next` is encouraged for a smoother migration."]
  let cast_address (type a b) (a : address) : (a, b) typed_address = [%external ("TEST_CAST_ADDRESS", a)]

  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration."]
  let to_address (type a b) (c : (a, b) typed_address) : address = [%external ("TEST_TO_ADDRESS", c)]

  (** Gets the storage of a typed account. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration."]
  let get_storage (type p s) (t : (p, s) typed_address) : s =
    let s : michelson_program = [%external ("TEST_GET_STORAGE", t)] in
    (decompile s : s)

  (** Gets the storage of an account in `michelson_program`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_storage` from `Test.Next` is encouraged for a smoother migration."]
  let get_storage_of_address (type b) (a : address) : b =
    (* we use unit bellow because we don't want inference to force useless annotations *)
    let a : (unit, b) typed_address = cast_address a in
    get_storage a

  (** Gets the balance of an account (given as an address) in tez. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration."]
  let get_balance_of_address (a : address) : tez = [%external ("TEST_GET_BALANCE", a)]

  (** Gets the balance of an account in tez. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration."]
  let get_balance (type p s) (a : (p, s) typed_address) : tez =
    [%external ("TEST_GET_BALANCE", to_address a)]

  (** Prints an string to stdout. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration."]
  let print (v : string) : unit = [%external ("TEST_PRINT", 1, v)]

  (** Prints an string to stderr. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `IO.eprint` from `Test.Next` is encouraged for a smoother migration."]
  let eprint (v : string) : unit = [%external ("TEST_PRINT", 2, v)]

  (** Return the voting power of a given contract. This voting power
    coincides with the weight of the contract in the voting listings
    (i.e., the rolls count) which is calculated at the beginning of
    every voting period. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration."]
  let get_voting_power (kh : key_hash) : nat = [%external ("TEST_GET_VOTING_POWER", kh)]

  (** Returns the address corresponding to the nth bootstrapped
    contract. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Account.Contract.bootstrap` from `Test.Next` is encouraged for a smoother migration."]
  let nth_bootstrap_contract (i : nat) : address = [%external ("TEST_NTH_BOOTSTRAP_CONTRACT", i)]

  (** Returns the address of the nth bootstrapped account. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration."]
  let nth_bootstrap_account (i : int) : address =
    let (a, _, _) = [%external ("TEST_GET_NTH_BS", i)] in
    a

  (** Returns the address, key and secret key of the nth bootstrapped
    account. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Account.info` from `Test.Next` is encouraged for a smoother migration."]
  let get_bootstrap_account (n : nat) : address * key * string = [%external ("TEST_GET_NTH_BS", (int n))]

  (** Returns the typed address corresponding to the nth bootstrapped
    contract currently loaded. The types are inferred from those
    contracts loaded with `Test.bootstrap_contract` (before reset). *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Account.Contract.bootstrap_typed_address` from `Test.Next` is encouraged for a smoother migration."]
  let nth_bootstrap_typed_address (type a b) (n : nat) : (a, b) typed_address = [%external ("TEST_NTH_BOOTSTRAP_TYPED_ADDRESS", n)]

  (** Returns addresses of orginated accounts in the last transfer. It
    is given in the form of a map binding the address of the source of
    the origination operation to the addresses of newly originated
    accounts. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.last_originations` from `Test.Next` is encouraged for a smoother migration."]
  let last_originations (u : unit) : (address, address list) map = [%external ("TEST_LAST_ORIGINATIONS", u)]

  (** This function creates a random value for a chosen type. *)
  let random (type a) (_u : unit) : a =
    let g : a pbt_gen = [%external ("TEST_RANDOM", false)] in
    [%external ("TEST_GENERATOR_EVAL", g)]

  (** Creates and returns secret key & public key of a new account. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration."]
  let new_account (u : unit) : string * key = [%external ("TEST_NEW_ACCOUNT", u)]

  (** It bakes until a number of cycles pass, so that an account
    registered as delegate can effectively act as a baker. Note: It
    can be used in tests to manually advance time. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.bake_until` from `Test.Next` is encouraged for a smoother migration."]
  let bake_until_n_cycle_end (n : nat) : unit = [%external ("TEST_BAKE_UNTIL_N_CYCLE_END", n)]

  let get_time (_u : unit) : timestamp = Tezos.get_now ()

  (** Registers a `key_hash` corresponding to an account as a delegate. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_delegate` from `Test.Next` is encouraged for a smoother migration."]
  let register_delegate (kh : key_hash) : unit = [%external ("TEST_REGISTER_DELEGATE", kh)]

  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.stake` from `Test.Next` is encouraged for a smoother migration."]
  let stake (kh : key_hash) (t : tez) : unit = [%external ("TEST_STAKE", kh, t)]

  (** Registers a global constant, returns its hash as a string. See
    the documentation for global constants for an example of usage. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_constant` from `Test.Next` is encouraged for a smoother migration."]
  let register_constant (m : michelson_program) : string = [%external ("TEST_REGISTER_CONSTANT", m)]

  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.to_typed_address` from `Test.Next` is encouraged for a smoother migration."]
  let to_typed_address (type a b) (c : a contract) : (a, b) typed_address = [%external ("TEST_TO_TYPED_ADDRESS", c)]

  (** Turn a constant (as a string) into a `michelson_program`. To be
    used together with `Test.register_constant`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.parse` from `Test.Next` is encouraged for a smoother migration."]
  let constant_to_michelson_program (s : string) : michelson_program = [%external ("TEST_CONSTANT_TO_MICHELSON", s)]

  (** Parses Michelson (as string) into a `michelson_program`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.parse` from `Test.Next` is encouraged for a smoother migration."]
  let parse_michelson (s : string) : michelson_program = [%external ("TEST_CONSTANT_TO_MICHELSON", s)]

  (** Pops a testing framework context from the stack of contexts, and
    sets it up as the new current context. In case the stack was
    empty, the current context is kept. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.restore` from `Test.Next` is encouraged for a smoother migration."]
  let restore_context (u : unit) : unit = [%external ("TEST_POP_CONTEXT", u)]

  (** Takes current testing framework context and saves it, pushing it
    into a stack of contexts. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.save` from `Test.Next` is encouraged for a smoother migration."]
  let save_context (u : unit) : unit = [%external ("TEST_PUSH_CONTEXT", u)]

  (** Drops a testing framework context from the stack of contexts. In
    case the stack was empty, nothing is done. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.drop` from `Test.Next` is encouraged for a smoother migration."]
  let drop_context (u : unit) : unit = [%external ("TEST_DROP_CONTEXT", u)]

  (** Converts a value to a string (same conversion as used by
    `Test.log`). *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration."]
  let to_string (type a) (v : a) : string = [%external ("TEST_TO_STRING", v, 0)]

  (** Converts a value to its JSON representation (as a string). *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `String.json` from `Test.Next` is encouraged for a smoother migration."]
  let to_json (type a) (v : a) : string = [%external ("TEST_TO_STRING", v, 1)]

  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `String.debugger_json` from `Test.Next` is encouraged for a smoother migration."]
  let to_debugger_json (type a) (v : a) : string = [%external ("TEST_TO_STRING", v, 2)]

  (** Forces the baking policy for `Test.transfer` and
    `Test.originate`. By default, the first bootstrapped account. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker_policy` from `Test.Next` is encouraged for a smoother migration."]
  let set_baker_policy (bp : test_baker_policy) : unit = [%external ("TEST_SET_BAKER", bp)]

  (** Forces the baker for `Test.transfer` and `Test.originate`,
    implemented using `Test.set_baker_policy` with `By_account`. By
    default, the first bootstrapped account. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker` from `Test.Next` is encouraged for a smoother migration."]
  let set_baker (a : address) : unit = set_baker_policy (By_account a)

  (** Measures the size of a contract. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.size` from `Test.Next` is encouraged for a smoother migration."]
  let size (type p s) (c : (p,s) michelson_contract) : int = [%external ("TEST_SIZE", c)]

  (** Compiles a contract from an entrypoint function. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.compile` from `Test.Next` is encouraged for a smoother migration."]
  let compile_contract (type p s) (f : p * s -> operation list * s) : (p,s) michelson_contract =
    let no_vs : s views = [%external ("TEST_NIL_VIEWS", ())] in
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT", f, no_vs)] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]

  (** Reads a contract from a `.tz` file. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.from_file` from `Test.Next` is encouraged for a smoother migration."]
  let read_contract_from_file (type p s) (fn : string) : (p,s) michelson_contract = [%external ("TEST_READ_CONTRACT_FROM_FILE", fn)]

  (** String consisting of the character represented by a `nat` in the
    interval `[0, 255]`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `String.chr` from `Test.Next` is encouraged for a smoother migration."]
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

  (** String consisting of only a newline. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `String.nl` from `Test.Next` is encouraged for a smoother migration."]
  let nl = [%external ("TEST_UNESCAPE_STRING", "\n")]

  (** Prints an string to stdout, ended with a newline. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration."]
  let println (v : string) : unit =
    print (v ^ nl)
  (* one day we might be able to write  `[@private] let print_values : ref bool = true` or something *)

  (** Turns on the printing of `test` prefixed values at the end of
    tests. This is the default behaviour. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration."]
  let set_print_values (_ : unit) : unit = let _ = [%external ("TEST_SET_PRINT_VALUES", true)] in ()

  (** Turns off the printing of `test` prefixed values at the end of
    tests. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `IO.unset_test_print` from `Test.Next` is encouraged for a smoother migration."]
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

  (** Returns the list of all the event payloads emited with a given
    tag by a given address. Any call to this function must be
    annotated with the expected payload type. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.last_events` from `Test.Next` is encouraged for a smoother migration."]
  let get_last_events_from (type a p s) (addr : (p,s) typed_address) (rtag: string) : a list =
    let addr = Tezos.address (to_contract addr) in
    let event_map : (address * a) list = [%external ("TEST_LAST_EVENTS", rtag)] in
    let f ((acc, (c_addr,event)) : a list * (address * a)) : a list =
      if addr = c_addr then event::acc
      else acc
    in
    List.fold f event_map ([]: a list)

  (** Bakes a transaction by sending an amount of tez with a parameter
    from the current source to another account. Returns the amount of
    gas consumed by the execution of the contract. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration."]
  let transfer (type p s) (a : (p,s) typed_address) (s : p) (t : tez) : test_exec_result =
    let a = to_contract a in
    let s : michelson_program = eval s in
    [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", a, (None : string option), s, t)]

  (** Bakes a transaction by sending an amount of tez with a parameter
    from the current source to another account. Returns the amount of
    gas consumed by the execution of the contract. Similar as
    `Test.transfer`, but fails when anything goes wrong. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration."]
  let transfer_exn (type p s) (a : (p,s) typed_address) (s : p) (t : tez) : nat =
    let a = to_contract a in
    let s : michelson_program = eval s in
    [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS_EXN", a, (None : string option), s, t)]
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration."]

  (** Logs a value. *)
  let log (type a) (v : a) : unit =
    let nl = [%external ("TEST_UNESCAPE_STRING", "\n")] in
    let s = to_string v ^ nl in
    print s

  (** Generates a number of random bootstrapped accounts with a
    default amount of `4000000` tez. The passed list can be used to
    overwrite the amount. By default, the state only has two
    bootstrapped accounts. Notice that since Ithaca, a percentage of
    an account's balance is frozen (5% in testing mode) in case the
    account can be taken to be a validator, and thus
    `Test.get_balance` can show a different amount to the one being
    set with `Test.reset_state`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration."]
  let reset_state (n : nat) (l : tez list) : unit = [%external ("TEST_STATE_RESET", (None : timestamp option), n, l)]

  (** Generates a number of random bootstrapped accounts with a
    default amount of `4000000` tez. The passed list can be used to
    overwrite the amount. By default, the state only has two
    bootstrapped accounts. Notice that since Ithaca, a percentage of
    an account's balance is frozen (5% in testing mode) in case the
    account can be taken to be a validator, and thus
    `Test.get_balance` can show a different amount to the one being
    set with `Test.reset_state`. It also takes a starting timestamp
    for the genesis block. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration."]
  let reset_state_at (t:timestamp) (n : nat) (l : tez list) : unit = [%external ("TEST_STATE_RESET", (Some t), n, l)]

  (** Setup a bootstrap contract with an entrypoint function, initial
    storage and initial balance. Bootstrap contracts will be loaded in
    order, and they will be available only after reset. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.Reset.add_func_contract` from `Test.Next` is encouraged for a smoother migration."]
  let bootstrap_contract (type p s) (f : p * s -> operation list * s) (s : s) (t : tez) : unit = [%external ("TEST_BOOTSTRAP_CONTRACT", f, s, t)]

  (** Mutates a value using a natural number as an index for the
    available mutations, returns an option for indicating whether
    mutation was successful or not. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.value` from `Test.Next` is encouraged for a smoother migration."]
  let mutate_value (type a) (n : nat) (v : a) : (a * mutation) option = [%external ("TEST_MUTATE_VALUE", n, v)]

  (** This function reconstructs a file from a mutation (second
    argument), and saves it to a file in the directory path (first
    argument). It returns an optional string indicating the filename
    where the mutation was saved, or `None` if there was an error. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.save` from `Test.Next` is encouraged for a smoother migration."]
  let save_mutation (s : string) (m : mutation) : string option = [%external ("TEST_SAVE_MUTATION", s, m)]

  (** Creates a signature of bytes from a string representing a secret
    key, it can be checked with `Crypto.check`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Crypto.sign` from `Test.Next` is encouraged for a smoother migration."]
  let sign (sk : string) (d : bytes) : signature = [%external ("TEST_SIGN", sk, d)]

  (** Adds an account specfied by secret key & public key to the test
    context. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Account.add` from `Test.Next` is encouraged for a smoother migration."]
  let add_account (s : string) (k : key) : unit = [%external ("TEST_ADD_ACCOUNT", s, k)]

  (** Adds an account `(sk, pk)` as a baker. The change is only
    effective after `Test.reset_state`. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.Reset.add_baker` from `Test.Next` is encouraged for a smoother migration."]
  let baker_account (p : string * key) (o : tez option) : unit = [%external ("TEST_BAKER_ACCOUNT", p, o)]

  (** The testing framework keeps an internal reference to the values
    corresponding to big map identifiers. This function allows to
    override the value of a particular big map identifier. It should
    not be normally needed, except in particular circumstances such as
    using custom bootstrap contracts that initialize big maps. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_big_map` from `Test.Next` is encouraged for a smoother migration."]
  let set_big_map (type a b) (i : int) (m : (a, b) big_map) : unit = [%external ("TEST_SET_BIG_MAP", i, m)]

  (** Bake a transaction by sending an amount of tez with a parameter
    from the current source to a contract. Returns the amount of gas
    consumed by the execution of the contract. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration."]
  let transfer_to_contract (type p) (c : p contract) (s : p) (t : tez) : test_exec_result =
    let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
    let s : michelson_program = eval s in
    [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS", c, e, s, t)]

  (** Bakes a transaction by sending an amount of tez with a parameter
    from the current source to a contract. Returns the amount of gas
    consumed by the execution of the contract. Similar as
    `Test.transfer_to_contract`, but fails when anything goes
    wrong. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration."]
  let transfer_to_contract_exn (type p) (c : p contract) (s : p) (t : tez) : nat =
      let e : string option = [%external ("TEST_GET_ENTRYPOINT", c)] in
      let s : michelson_program = eval s in
      [%external ("TEST_EXTERNAL_CALL_TO_ADDRESS_EXN", c, e, s, t)]

  (** Compares two Michelson values. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration."]
  let michelson_equal (m1 : michelson_program) (m2 : michelson_program) : bool = m1 = m2

  (** Gets the contract corresponding to an entrypoint of a typed
    address: the contract parameter in the result will be the type of
    the entrypoint, it needs to be annotated, entrypoint string should
    omit the prefix "%", but if passed a string starting with "%", it
    will be removed (and a warning emitted). *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_entrypoint` from `Test.Next` is encouraged for a smoother migration."]
  let to_entrypoint (type a b c) (s : string) (t : (a, b) typed_address) : c contract =
    let s = if String.length s > 0n then
              if String.sub 0n 1n s = "%" then
                let () = log "WARNING: Test.to_entrypoint: automatically removing starting %" in
                String.sub 1n (abs (String.length s - 1)) s
	      else s
	    else s in
    [%external ("TEST_TO_ENTRYPOINT", s, t)]
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Dynamic_entrypoints.storage` from `Test.Next` is encouraged for a smoother migration."]

  let storage_with_dynamic_entrypoints (type p s s2) ((_, _, init_opt) : (p, s) module_contract) (s:s2) =
    type t = [@layout comb] { storage : s2 ; dynamic_entrypoints : dynamic_entrypoints} in
    match init_opt with
    | Some dynamic_entrypoints ->
      ({storage = s ; dynamic_entrypoints } : t)
    | None -> failwith "Your contract do not have dynamic entrypoints"

  (** Originate a contract with initial storage and initial
    balance. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.michelson` from `Test.Next` is encouraged for a smoother migration."]
  let originate_contract (type p s) (c : (p,s) michelson_contract) (s : s) (t : tez) : (p,s) typed_address =
    let s = eval s in
    [%external ("TEST_ORIGINATE", c, s, t)]

  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.compile_with_views` from `Test.Next` is encouraged for a smoother migration."]
  let compile_contract_with_views (type p s) (f : p * s -> operation list * s) (vs : s views) : (p,s) michelson_contract =
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT", f, vs)] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]

  (** Originate a contract with an entrypoint function in curried
    form, initial storage and initial balance. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration."]
  let originate (type p s) ((f, vs, _) : (p, s) module_contract) (s : s) (t : tez) : (p, s) origination_result =
    let code = compile_contract_with_views f vs in
    let addr = originate_contract code s t in
    let size = size code in
    { addr ; code ; size }

  (** Compiles a contract with a path to the contract file, an
    entrypoint, and a list of views. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.from_file` from `Test.Next` is encouraged for a smoother migration."]
  let compile_contract_from_file (type p s) (fn : string) : (p,s) michelson_contract =
    let ast_c : (p,s) michelson_contract = [%external ("TEST_COMPILE_CONTRACT_FROM_FILE", fn, (None : nat option))] in
    [%external ("TEST_COMPILE_AST_CONTRACT", ast_c)]

  (** Originate a contract with a path to the contract file, an
    entrypoint, and a list of views, together with an initial storage
    and an initial balance. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration."]
  let originate_from_file (type p s) (fn : string) (s : s)  (t : tez) : (p, s) origination_result =
    let code = compile_contract_from_file fn in
    let addr = originate_contract code s t in
    let size = size code in
    { addr ; code ; size }

  (* Mutations *)

  (** Given a value to mutate (first argument), it will try all the
    mutations available of it, passing each one to the function
    (second argument). On the first case of non failure when running
    the function on a mutation, the value and mutation involved will
    be returned. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.func` from `Test.Next` is encouraged for a smoother migration."]
  let mutation_test (type a b) (v : a) (tester : a -> b) : (b * mutation) option =
    let try_with (type a) (v : unit -> a) (c : unit -> a) = [%external ("TEST_TRY_WITH", v, c)] in
    type ret_code = Passed of (b * mutation) | Continue | Stop in
    let rec mutation_nth (n : nat) : (b * mutation) option =
      let curr = match mutate_value n v with
        | Some (v, m) -> try_with (fun () -> let b = tester v in Passed (b, m)) (fun () -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> None
      | Continue -> mutation_nth (n + 1n)
      | Passed (b, m) -> Some (b, m) in
    mutation_nth 0n

  (** Given a value to mutate (first argument), it will try all the
    mutations of it, passing each one to the function (second
    argument). In case no failure arises when running the function on
    a mutation, the failure and mutation involved will be added to the
    list to be returned. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.func` from `Test.Next` is encouraged for a smoother migration."]
  let mutation_test_all (type a b) (v : a) (tester : a -> b) : (b * mutation) list =
    let try_with (type a) (v : unit -> a) (c : unit -> a) = [%external ("TEST_TRY_WITH", v, c)] in
    type ret_code = Passed of (b * mutation) | Continue | Stop in
    let rec mutation_nth (acc : (b * mutation) list) (n : nat) : (b * mutation) list =
      let curr = match mutate_value n v with
        | Some (v, m) -> try_with (fun () -> let b = tester v in Passed (b, m)) (fun () -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> acc
      | Continue -> mutation_nth acc (n + 1n)
      | Passed (b, m) -> mutation_nth ((b, m) :: acc) (n + 1n) in
    mutation_nth ([] : (b * mutation) list) 0n

  (** Given a contract from a file (passed by filepath, entrypoint and
    views), an initial storage and balance, it will originate mutants
    of the contract and pass the result to the function (last
    argument). On the first case of non failure when running the
    function on a mutation, the value and mutation involved will be
    returned. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.from_file` from `Test.Next` is encouraged for a smoother migration."]
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
        | Some (v, m) -> try_with (fun () -> let b = wrap_tester v in Passed (b, m)) (fun () -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> None
      | Continue -> mutation_nth (n + 1n)
      | Passed (b, m) -> Some (b, m) in
    mutation_nth 0n

  (** Given a contract from a file (passed by filepath, entrypoint and
    views), an initial storage and balance, it will originate mutants
    of the contract and pass the result to the function (last
    argument). In case no failure arises when running the function on
    a mutation, the failure and mutation involved will be added to the
    list to be returned. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.from_file` from `Test.Next` is encouraged for a smoother migration."]
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
        | Some (v, m) -> try_with (fun () -> let b = wrap_tester v in Passed (b, m)) (fun () -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> acc
      | Continue -> mutation_nth acc (n + 1n)
      | Passed (b, m) -> mutation_nth ((b, m) :: acc) (n + 1n) in
    mutation_nth ([] : (b * mutation) list) 0n

  (** Given a contract as a module/namespace, an initial storage and
    balance, it will originate mutants of the contract and pass the
    result to the function (last argument). On the first case of non
    failure when running the function on a mutation, the value and
    mutation involved will be returned. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.contract` from `Test.Next` is encouraged for a smoother migration."]
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
        | Some (v, m) -> try_with (fun () -> let b = wrap_tester v in Passed (b, m)) (fun () -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> None
      | Continue -> mutation_nth (n + 1n)
      | Passed (b, m) -> Some (b, m) in
    mutation_nth 0n

  (** Given a contract as a module/namespace, an initial storage and
    balance, it will originate mutants of the contract and pass the
    result to the function (last argument). In case no failure arises
    when running the function on a mutation, the failure and mutation
    involved will be added to the list to be returned. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.contract` from `Test.Next` is encouraged for a smoother migration."]
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
        | Some (v, m) -> try_with (fun () -> let b = wrap_tester v in Passed (b, m)) (fun () -> Continue)
        | None -> Stop in
      match curr with
      | Stop -> acc
      | Continue -> mutation_nth acc (n + 1n)
      | Passed (b, m) -> mutation_nth ((b, m) :: acc) (n + 1n) in
    mutation_nth ([] : (b * mutation) list) 0n

  (* Assertions *)

  (** display-only-for-cameligo
    The call `assert cond` terminates the execution with the string
    `"failed assertion"` if, and only if, the boolean condition `cond`
    is false. The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter. *)
  (** display-only-for-jsligo
    The call `assert(cond)` terminates the execution with the string
    `"failed assertion"` if, and only if, the boolean condition `cond`
    is false. The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration."]
  let assert (b : bool) : unit = if b then () else failwith "failed assertion"

  (** display-only-for-cameligo
    The call `assert_some opt` terminates the execution with the
    string `"failed assert some"` if, and only if, `opt` is `None`.
    The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter. *)
  (** display-only-for-jsligo
    The call `assert_some(opt)` terminates the execution with the
    string `"failed assert some"` if, and only if, `opt` is `None()`.
    The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.some` from `Test.Next` is encouraged for a smoother migration."]
  let assert_some (type a) (v : a option) : unit = match v with | None -> failwith "failed assert some" | Some _ -> ()

  (** display-only-for-cameligo
    The call `assert_none opt` terminates the execution with the string
    `"failed assert none"` if, and only if, `opt` is not `None`.
    The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter. *)
  (** display-only-for-jsligo
    The call `assert_none(opt)` terminates the execution with the string
    `"failed assert none"` if, and only if, `opt` is not `None()`.
    The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.none` from `Test.Next` is encouraged for a smoother migration."]
  let assert_none (type a) (v : a option) : unit = match v with | None -> () | Some _ -> failwith "failed assert none"

  (** display-only-for-cameligo
    The call `assert_with_error cond error` terminates the execution
    with the string `error` (that is, an error message) if, and only
    if, the boolean condition `cond` is false. The failure is handled
    by LIGO's testing framework and not by Michelson's interpreter. *)
  (** display-only-for-jsligo
    The call `assert_with_error(cond, error)` terminates the execution
    with the string `error` (that is, an error message) if, and only
    if, the boolean condition `cond` is false. The failure is handled
    by LIGO's testing framework and not by Michelson's interpreter. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.Error.assert` from `Test.Next` is encouraged for a smoother migration."]
  let assert_with_error (b : bool) (s : string) = if b then () else failwith s

  (** display-only-for-cameligo
    The call `assert_some_with_error opt err` terminates the execution
    with the string `err` (that is, an error message) if, and only if,
    `opt` is `None`. The failure is handled by LIGO's testing
    framework and not by Michelson's interpreter. *)
  (** display-only-for-jsligo
    The call `assert_some_with_error(opt, err)` terminates the
    execution with the string `err` (that is, an error message) if,
    and only if, `opt` is `None()`. The failure is handled by LIGO's
    testing framework and not by Michelson's interpreter. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.Error.some` from `Test.Next` is encouraged for a smoother migration."]
  let assert_some_with_error (type a) (v : a option) (s : string) : unit = match v with | None -> failwith s | Some _ -> ()

  (** display-only-for-cameligo
    The call `assert_none_with_error opt err` terminates the execution
    with the string `err` (that is, an error message) if, and only if,
    `opt` is an optional value different from `None`. The failure is
    handled by LIGO's testing framework and not by Michelson's
    interpreter. *)
  (** display-only-for-jsligo
    The call `assert_none_with_error(opt, err)` terminates the
    execution with the string `err` (that is, an error message) if,
    and only if, `opt` is an optional value different from
    `None()`. The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.Error.none` from `Test.Next` is encouraged for a smoother migration."]
  let assert_none_with_error (type a) (v : a option) (s : string) : unit = match v with | None -> () | Some _ -> failwith s

  (* Comparisons *)

  [@private] let compare (type a) (lhs : a) (rhs : a) : int = [%external ("TEST_COMPARE", lhs, rhs)]

  (** display-only-for-cameligo
    The call `equal x y` returns `true` if, and only if, `x` and `y`
    are considered to be equal w.r.t. the order on the underlying
    type. *)
  (** display-only-for-jsligo
    The call `equal(x, y)` returns `true` if, and only if, `x` and `y`
    are considered to be equal w.r.t. the order on the underlying
    type. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration."]
  let equal (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs = 0

  (** display-only-for-cameligo
    The call `not_equal x y` returns `true` if, and only if, `x` and
    `y` are not considered to be equal w.r.t. the order on the
    underlying type. *)
  (** display-only-for-jsligo
    The call `not_equal(x, y)` returns `true` if, and only if, `x` and
    `y` are not considered to be equal w.r.t. the order on the
    underlying type. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.neq` from `Test.Next` is encouraged for a smoother migration."]
  let not_equal (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs <> 0

  (** display-only-for-cameligo
    The call `greater x y` returns `true` if, and only if, `x` is
    considered to be greater than `y` w.r.t. the order on the
    underlying type. *)
  (** display-only-for-jsligo
    The call `greater(x, y)` returns `true` if, and only if, `x` is
    considered to be greater than `y` w.r.t. the order on the
    underlying type. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.gt` from `Test.Next` is encouraged for a smoother migration."]
  let greater (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs > 0

  (** display-only-for-cameligo
    The call `less x y` returns `true` if, and only if, `x` is
    considered to be less than `y` w.r.t. the order on the underlying
    type. *)
  (** display-only-for-jsligo
    The call `less(x, y)` returns `true` if, and only if, `x` is
    considered to be less than `y` w.r.t. the order on the underlying
    type. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.lt` from `Test.Next` is encouraged for a smoother migration."]
  let less (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs < 0

  (** display-only-for-cameligo
    The call `greater_or_equal x y` returns `true` if, and only if,
    `x` is considered to be greater or equal than `y` w.r.t. the order
    on the underlying type. *)
  (** display-only-for-jsligo
    The call `greater_or_equal(x, y)` returns `true` if, and only if,
    `x` is considered to be greater or equal than `y` w.r.t. the order
    on the underlying type. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.ge` from `Test.Next` is encouraged for a smoother migration."]
  let greater_or_equal (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs >= 0

  (** display-only-for-cameligo
    The call `less_or_equal x y` returns `true` if, and only if, `x`
    is considered to be less or equal than `y` w.r.t. the order on the
    underlying type. *)
  (** display-only-for-jsligo
    The call `less_or_equal(x, y)` returns `true` if, and only if, `x`
    is considered to be less or equal than `y` w.r.t. the order on the
    underlying type. *)
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.le` from `Test.Next` is encouraged for a smoother migration."]
  let less_or_equal (type a) (lhs : a) (rhs : a) : bool = compare lhs rhs <= 0

  (* Timelocks *)

  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create` from `Test.Next` is encouraged for a smoother migration."]
  let create_chest (b : bytes) (n : nat) : chest * chest_key = [%external ("TEST_CREATE_CHEST", b, n)]
  [@deprecated "In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create_key` from `Test.Next` is encouraged for a smoother migration."]
  let create_chest_key (c : chest) (n : nat) : chest_key = [%external ("TEST_CREATE_CHEST_KEY", c, n)]

  module Proxy_ticket = struct
    [@private]
    let proxy_transfer_contract
      (type vt whole_p)
      (mk_param : vt ticket -> whole_p)
      (p        : (vt * nat) * address)
      (()       : unit)
      : operation list * unit =
      let ((v,amt),dst_addr) = p in
      let ticket = Option.unopt (Tezos.create_ticket v amt) in
      let tx_param = mk_param ticket in
      let c : whole_p contract = Tezos.get_contract_with_error dst_addr "Testing proxy: you provided a wrong address" in
      let op = Tezos.transaction tx_param 1mutez c
      in [op], ()

    [@private]
    let proxy_originate_contract
      (type vt whole_s vp)
      (mk_storage : vt ticket -> whole_s)
      (main       : vp -> whole_s -> operation list * whole_s)
      (p          : (vt * nat))
      (_          : address option)
      : operation list * address option =
      let (v,amt) = p in
      let ticket = Option.unopt (Tezos.create_ticket v amt) in
      let init_storage : whole_s = mk_storage ticket in
      let op,addr =
        Tezos.create_contract main (None: key_hash option) 0mutez init_storage
      in [op], Some addr

    [@private]
    let originate_from_function
      (type p s)
      (f : p -> s -> operation list * s)
      (s : s)
      (t : tez)
      : (p, s) typed_address =
      let code = compile_contract (uncurry f)
      in originate_contract code s t

    type 'v proxy_address = (('v * nat) * address , unit) typed_address

    let init_transfer
      (type vt whole_p)
      (mk_param: vt ticket -> whole_p)
      : vt proxy_address =
      let proxy_transfer
        : (vt * nat) * address -> unit -> operation list * unit =
        proxy_transfer_contract mk_param
      in originate_from_function proxy_transfer () 1tez

    let transfer
      (type vt)
      (taddr_proxy : vt proxy_address)
      (info        : (vt * nat) * address)
      : test_exec_result =
      let ticket_info, dst_addr = info in
      transfer_to_contract
        (to_contract taddr_proxy)
        (ticket_info , dst_addr)
        1mutez

    let originate
      (type vt whole_s vp)
      (ticket_info : vt * nat)
      (mk_storage  : vt ticket -> whole_s)
      (contract    : vp -> whole_s -> operation list * whole_s)
      : (vp,whole_s) typed_address =
      let proxy_origination
        : vt * nat -> address option -> operation list * address option =
        proxy_originate_contract mk_storage contract
      in
      let taddr =
        originate_from_function
          proxy_origination (None : address option) 1tez in
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
      (** Given a value to mutate (first argument), it will try all the
        mutations available of it, passing each one to the function
        (second argument). On the first case of non failure when running
        the function on a mutation, the value and mutation involved will
        be returned. *)
      let func = mutation_test

      (** Given a contract from a file (passed by filepath, entrypoint and
        views), an initial storage and balance, it will originate mutants
        of the contract and pass the result to the function (last
        argument). On the first case of non failure when running the
        function on a mutation, the value and mutation involved will be
        returned. *)
      let from_file = originate_from_file_and_mutate

      (** Given a contract as a module/namespace, an initial storage and
        balance, it will originate mutants of the contract and pass the
        result to the function (last argument). On the first case of non
        failure when running the function on a mutation, the value and
        mutation involved will be returned. *)
      let contract = originate_module_and_mutate

      module All = struct
          (** Given a value to mutate (first argument), it will try all the
             mutations of it, passing each one to the function (second
             argument). In case no failure arises when running the function on
             a mutation, the failure and mutation involved will be added to the
             list to be returned. *)
          let func = mutation_test_all

          (** Given a contract from a file (passed by filepath, entrypoint and
            views), an initial storage and balance, it will originate mutants
            of the contract and pass the result to the function (last
            argument). In case no failure arises when running the function on
            a mutation, the failure and mutation involved will be added to the
            list to be returned. *)
          let from_file = originate_from_file_and_mutate_all

          (** Given a contract as a module/namespace, an initial storage and
            balance, it will originate mutants of the contract and pass the
            result to the function (last argument). In case no failure arises
            when running the function on a mutation, the failure and mutation
            involved will be added to the list to be returned. *)
          let contract = originate_and_mutate_all
      end

      (** Mutates a value using a natural number as an index for the
        available mutations, returns an option for indicating whether
        mutation was successful or not. *)
      let value (type a) (n : nat) (v : a) : (a * mutation) option =
        [%external ("TEST_MUTATE_VALUE", n, v)]

      (** This function reconstructs a file from a mutation (second
        argument), and saves it to a file in the directory path (first
        argument). It returns an optional string indicating the filename
        where the mutation was saved, or `None` if there was an error. *)
      let save (s : string) (m : mutation) : string option =
        [%external ("TEST_SAVE_MUTATION", s, m)]
    end
    module PBT = PBT
    module State = struct
      (** Pops a testing framework context from the stack of contexts, and
        sets it up as the new current context. In case the stack was
        empty, the current context is kept. *)
      let restore = restore_context

      (** Takes current testing framework context and saves it, pushing it
        into a stack of contexts. *)
      let save = save_context

      (** Drops a testing framework context from the stack of contexts. In
        case the stack was empty, nothing is done. *)
      let drop = drop_context

      (** Generates a number of random bootstrapped accounts with a
        default amount of `4000000` tez. The passed list can be used to
        overwrite the amount. By default, the state only has two
        bootstrapped accounts. Notice that since Ithaca, a percentage of
        an account's balance is frozen (5% in testing mode) in case the
        account can be taken to be a validator, and thus getting
        balance can show a different amount to the one being set with
        `Test.State.reset`. *)
      let reset = reset_state

      (** Generates a number of random bootstrapped accounts with a
        default amount of `4000000` tez. The passed list can be used to
        overwrite the amount. By default, the state only has two
        bootstrapped accounts. Notice that since Ithaca, a percentage of
        an account's balance is frozen (5% in testing mode) in case the
        account can be taken to be a validator, and thus getting
        balance can show a different amount to the one being set with
        `Test.State.reset`. It also takes a starting timestamp
        for the genesis block. *)
      let reset_at = reset_state_at

      (** Registers a `key_hash` corresponding to an account as a delegate. *)
      let register_delegate = register_delegate


      (** Registers a global constant, returns its hash as a string. See
        the documentation for global constants for an example of usage. *)
      let register_constant = register_constant

      (** Sets the source for `Test.transfer` and `Test.originate`. *)
      let set_source (a : address) : unit =
        [%external ("TEST_SET_SOURCE", a)]

      (** Forces the baking policy for `Test.transfer` and
        `Test.originate`. By default, the first bootstrapped account. *)
      let set_baker_policy (bp : test_baker_policy) : unit =
        [%external ("TEST_SET_BAKER", bp)]

      (** Forces the baker for `Test.transfer` and `Test.originate`,
        implemented using `Test.set_baker_policy` with `By_account`. By
        default, the first bootstrapped account. *)
      let set_baker (a : address) : unit =
        set_baker_policy (By_account a)

      (** It bakes until a number of cycles pass, so that an account
       registered as delegate can effectively act as a baker. Note: It
       can be used in tests to manually advance time. *)
      let bake_until (n : nat) : unit =
        [%external ("TEST_BAKE_UNTIL_N_CYCLE_END", n)]

      (** The testing framework keeps an internal reference to the values
        corresponding to big map identifiers. This function allows to
        override the value of a particular big map identifier. It should
        not be normally needed, except in particular circumstances such as
        using custom bootstrap contracts that initialize big maps. *)
      let set_big_map (type k v) (i : int) (m : (k, v) big_map) : unit =
        [%external ("TEST_SET_BIG_MAP", i, m)]

      (** Return the voting power of a given contract. This voting power
        coincides with the weight of the contract in the voting listings
        (i.e., the rolls count) which is calculated at the beginning of
        every voting period. *)
      let get_voting_power (kh : key_hash) : nat =
        [%external ("TEST_GET_VOTING_POWER", kh)]

      (** Returns the total voting power of all contracts. The total
        voting power coincides with the sum of the rolls count of every
        contract in the voting listings. The voting listings is calculated
        at the beginning of every voting period. *)
      let get_total_voting_power (() : unit) : nat =
        [%external ("TEST_GET_TOTAL_VOTING_POWER", ())]

      (** Returns addresses of orginated accounts in the last transfer. It
        is given in the form of a map binding the address of the source of
        the origination operation to the addresses of newly originated
        accounts. *)
      let last_originations (() : unit) : (address, address list) map =
        [%external ("TEST_LAST_ORIGINATIONS", ())]

      (** Returns the list of all the event payloads emited with a given
        tag by a given address. Any call to this function must be
        annotated with the expected payload type. *)
      let last_events (type a p s)
        (addr : (p,s) typed_address)
        (rtag: string)
      : a list =
        let addr = Tezos.address (to_contract addr) in
        let event_map : (address * a) list = [%external ("TEST_LAST_EVENTS", rtag)] in
        let f ((acc, (c_addr,event)) : a list * (address * a)) : a list =
          if addr = c_addr then event::acc
          else acc
        in
        List.fold f event_map ([]: a list)

      let stake (kh : key_hash) (t : tez) : unit =
        [%external ("TEST_STAKE", kh, t)]

      module Reset = struct
        (** Adds an account `(sk, pk)` as a baker. The change is only
          effective after `Test.reset_state`. *)
        let add_baker (p : string * key) (o : tez option) : unit =
          [%external ("TEST_BAKER_ACCOUNT", p, o)]

        (** Setup a bootstrap contract with an entrypoint function, initial
          storage and initial balance. Bootstrap contracts will be loaded in
          order, and they will be available only after reset. *)
        let add_func_contract (type p s)
          (f : p * s -> operation list * s)
          (s : s)
          (t : tez)
        : unit =
          [%external ("TEST_BOOTSTRAP_CONTRACT", f, s, t)]
      end
    end
    module Account = struct
      (** Returns the address of the nth bootstrapped account. *)
      let address (n : nat) =
        let (a, _, _) = [%external ("TEST_GET_NTH_BS", (int n))] in
        a

      (** Returns the address of the 0th bootstrapped account. *)
      let alice () = address 0n

      (** Returns the address of the 1st bootstrapped account. *)
      let bob () = address 1n

      (** Returns the address of the 2nd bootstrapped account. *)
      let carol () = address 2n

      (** Returns the address of the 3rd bootstrapped account. *)
      let dan () = address 3n

      (** Adds an account specfied by secret key & public key to the test
        context. *)
      let add = add_account

      type info = { addr: address; pk: key; sk: string }

      (** Returns the address information of the nth bootstrapped
        account. *)
      let info (n : nat) : info =
        let (addr, pk, sk) = get_bootstrap_account n in
        { addr ; pk ; sk }

      (** Creates and returns information of a new account. *)
      let new (() : unit) : info =
        let (sk, pk) = [%external ("TEST_NEW_ACCOUNT", ())] in
        let addr = [%michelson ({| { HASH_KEY ; ADDRESS } |} pk : address)] in
        { addr ; pk ; sk }

      module Contract = struct
        (** Returns the address corresponding to the nth bootstrapped
          contract. *)
        let bootstrap (i : nat) : address =
          [%external ("TEST_NTH_BOOTSTRAP_CONTRACT", i)]

        (** Returns the typed address corresponding to the nth bootstrapped
          contract currently loaded. The types are inferred from those
          contracts loaded with `Test.State.Reset.add_func_contract`
	  (before reset). *)
        let bootstrap_typed_address (type a b) (n : nat) : (a, b) typed_address =
          [%external ("TEST_NTH_BOOTSTRAP_TYPED_ADDRESS", n)]
      end
    end
    module Compare = struct
      [@private]
      let compare (type a) (lhs : a) (rhs : a) : int =
        [%external ("TEST_COMPARE", lhs, rhs)]

      (** display-only-for-cameligo
        The call `eq x y` returns `true` if, and only if, `x` and `y`
        are considered to be equal w.r.t. the order on the underlying
        type. *)
      (** display-only-for-jsligo
        The call `eq(x, y)` returns `true` if, and only if, `x` and `y`
        are considered to be equal w.r.t. the order on the underlying
        type. *)
      let eq (type a) (lhs : a) (rhs : a) : bool =
        compare lhs rhs = 0

      (** display-only-for-cameligo
        The call `neq x y` returns `true` if, and only if, `x` and
        `y` are not considered to be equal w.r.t. the order on the
        underlying type. *)
      (** display-only-for-jsligo
        The call `neq(x, y)` returns `true` if, and only if, `x` and
        `y` are not considered to be equal w.r.t. the order on the
        underlying type. *)
      let neq (type a) (lhs : a) (rhs : a) : bool =
        compare lhs rhs <> 0

      (** display-only-for-cameligo
        The call `gt x y` returns `true` if, and only if, `x` is
        considered to be greater than `y` w.r.t. the order on the
        underlying type. *)
      (** display-only-for-jsligo
        The call `gt(x, y)` returns `true` if, and only if, `x` is
        considered to be greater than `y` w.r.t. the order on the
        underlying type. *)
      let gt (type a) (lhs : a) (rhs : a) : bool =
        compare lhs rhs > 0

      (** display-only-for-cameligo
        The call `lt` returns `true` if, and only if, `x` is
        considered to be less than `y` w.r.t. the order on the underlying
        type. *)
      (** display-only-for-jsligo
        The call `lt(x, y)` returns `true` if, and only if, `x` is
        considered to be less than `y` w.r.t. the order on the underlying
        type. *)
      let lt (type a) (lhs : a) (rhs : a) : bool =
        compare lhs rhs < 0


      (** display-only-for-cameligo
        The call `ge x y` returns `true` if, and only if,
        `x` is considered to be greater or equal than `y` w.r.t. the order
        on the underlying type. *)
      (** display-only-for-jsligo
        The call `ge(x, y)` returns `true` if, and only if,
        `x` is considered to be greater or equal than `y` w.r.t. the order
        on the underlying type. *)
      let ge (type a) (lhs : a) (rhs : a) : bool =
        compare lhs rhs >= 0

      (** display-only-for-cameligo
        The call `le x y` returns `true` if, and only if, `x`
        is considered to be less or equal than `y` w.r.t. the order on the
        underlying type. *)
      (** display-only-for-jsligo
        The call `le(x, y)` returns `true` if, and only if, `x`
        is considered to be less or equal than `y` w.r.t. the order on the
        underlying type. *)
      let le (type a) (lhs : a) (rhs : a) : bool =
        compare lhs rhs <= 0
    end
    module Michelson = struct
      (** Run a function on an input, all in Michelson. More concretely:
        a) compiles the function argument to Michelson `f_mich`; b)
        compiles the value argument (which was evaluated already) to
        Michelson `v_mich`; c) runs the Michelson interpreter on the code
        `f_mich` with starting stack `[v_mich]`. *)
      let run = run

      (** Compile a LIGO value to Michelson. Currently it is a
        renaming of `compile_value`. *)
      let eval = eval

      (** Decompile a Michelson value to LIGO, following the
        (mandatory) type annotation. Note: This operation can fail at
        run-time, in case that the `michelson_program` given cannot be
        decompiled to something compatible with the annotated type. *)
      let decompile = decompile

      (** Parses Michelson (as string) into a `michelson_program`. *)
      let parse = parse_michelson

      module Contract = struct
        (** Compiles a contract from an entrypoint function. *)
        let compile = compile_contract

        let compile_with_views = compile_contract_with_views

        (** Measures the size of a contract. *)
        let size = size

        (** Reads a contract from a `.tz` file. *)
        let from_file = read_contract_from_file
      end
    end
    module IO = struct
      (** Prints an string to stdout. *)
      let print = print

      (** Prints an string to stdout, ended with a newline. *)
      let println = println

      (** Prints an string to stderr. *)
      let eprint = eprint

      (** Prints an string to stderr, ended with a newline. *)
      let eprintln (v : string) : unit =
        eprint (v ^ nl)

      (** Logs a value. *)
      let log = log

      (** Turns on the printing of `test` prefixed values at the end of
        tests. This is the default behaviour. *)
      let set_test_print = set_print_values

      (** Turns off the printing of `test` prefixed values at the end of
        tests. *)
      let unset_test_print = unset_print_values
    end
    module Assert = struct
      (** Cause the testing framework to fail. *)
      let failwith (type a b) (v : a) : b =
        [%external ("TEST_FAILWITH", v)]

      (** display-only-for-cameligo
        The call `assert cond` terminates the execution with the string
        `"failed assertion"` if, and only if, the boolean condition `cond`
        is false. The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter. *)
      (** display-only-for-jsligo
        The call `assert(cond)` terminates the execution with the string
        `"failed assertion"` if, and only if, the boolean condition `cond`
        is false. The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter. *)
      let assert = assert


      (** display-only-for-cameligo
        The call `some opt` terminates the execution with the
        string `"failed assert some"` if, and only if, `opt` is `None`.
        The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter. *)
      (** display-only-for-jsligo
        The call `some(opt)` terminates the execution with the
        string `"failed assert some"` if, and only if, `opt` is `None()`.
        The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter. *)
      let some = assert_some

      (** display-only-for-cameligo
        The call `none opt` terminates the execution with the string
        `"failed assert none"` if, and only if, `opt` is not `None`.
        The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter. *)
      (** display-only-for-jsligo
        The call `none(opt)` terminates the execution with the string
        `"failed assert none"` if, and only if, `opt` is not `None()`.
        The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter. *)
      let none = assert_none

      module Error = struct
        (** display-only-for-cameligo
          The call `assert cond error` terminates the execution
          with the string `error` (that is, an error message) if, and only
          if, the boolean condition `cond` is false. The failure is handled
          by LIGO's testing framework and not by Michelson's interpreter. *)
        (** display-only-for-jsligo
          The call `assert(cond, error)` terminates the execution
          with the string `error` (that is, an error message) if, and only
          if, the boolean condition `cond` is false. The failure is handled
          by LIGO's testing framework and not by Michelson's interpreter. *)
        let assert = assert_with_error

        (** display-only-for-cameligo
          The call `some opt err` terminates the execution
          with the string `err` (that is, an error message) if, and only if,
          `opt` is `None`. The failure is handled by LIGO's testing
          framework and not by Michelson's interpreter. *)
        (** display-only-for-jsligo
          The call `some(opt, err)` terminates the
          execution with the string `err` (that is, an error message) if,
          and only if, `opt` is `None()`. The failure is handled by LIGO's
          testing framework and not by Michelson's interpreter. *)
        let some = assert_some_with_error

        (** display-only-for-cameligo
          The call `none opt err` terminates the execution
          with the string `err` (that is, an error message) if, and only if,
          `opt` is an optional value different from `None`. The failure is
          handled by LIGO's testing framework and not by Michelson's
          interpreter. *)
        (** display-only-for-jsligo
          The call `none(opt, err)` terminates the
          execution with the string `err` (that is, an error message) if,
          and only if, `opt` is an optional value different from
          `None()`. The failure is handled by LIGO's testing framework and
          not by Michelson's interpreter. *)
        let none = assert_none_with_error
      end
    end
    module String = struct
      (** String consisting of the character represented by a `nat` in the
        interval `[0, 255]`. *)
      let chr = chr

      (** String consisting of only a newline. *)
      let nl = nl

      (** Converts a value to a string (same conversion as used by
        `Test.log`). *)
      let show = to_string

      (** Converts a value to its JSON representation (as a string). *)
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

      (** Originate a contract with an entrypoint function in curried
        form, initial storage and initial balance. *)
      let contract (type p s) (c : (p, s) module_contract) (s : s) (t : tez) : (p, s) origination_result =
        let { addr; code ; size } = originate c s t in
        let taddr = addr in
        { taddr ; code ; size }

      (** Originate a contract with a path to the contract file, an
        entrypoint, and a list of views, together with an initial storage
        and an initial balance. *)
      let from_file (type p s) (fn : string) (s : s)  (t : tez) : (p, s) origination_result =
        let { addr ; code ; size } = originate_from_file fn s t in
        let taddr = addr in
        { taddr ; code ; size }

      (** Originate a contract with initial storage and initial
        balance. *)
      let michelson (type p s)
        (c : (p,s) michelson_contract)
        (s : s)
        (t : tez)
      : (p,s) typed_address =
        let s = eval s in
        [%external ("TEST_ORIGINATE", c, s, t)]
    end
    module Contract = struct
      let transfer = transfer_to_contract
      let transfer_exn = transfer_to_contract_exn
      let to_typed_address (type p s) (c : p contract) : (p, s) typed_address =
        [%external ("TEST_TO_TYPED_ADDRESS", c)]
    end
    module Typed_address = struct
       (** Bakes a transaction by sending an amount of tez with a parameter
         from the current source to another account. Returns the amount of
         gas consumed by the execution of the contract. *)
      let transfer = transfer

      (** Bakes a transaction by sending an amount of tez with a parameter
        from the current source to another account. Returns the amount of
        gas consumed by the execution of the contract. Similar as
        `Test.transfer`, but fails when anything goes wrong. *)
      let transfer_exn = transfer_exn

      (** Gets the storage of a typed account. *)
      let get_storage = get_storage

      (** Gets the balance of an account in tez. *)
      let get_balance (type p s) (a : (p, s) typed_address) : tez =
        [%external ("TEST_GET_BALANCE", to_address a)]
      let to_address (type p s) (c : (p, s) typed_address) : address =
        [%external ("TEST_TO_ADDRESS", c)]

      (** Gets the contract corresponding to the default entrypoint of a
        typed address: the contract parameter in the result will be the
        type of the default entrypoint (generally `'param`, but this might
        differ if `'param` includes a "default" entrypoint). *)
      let to_contract (type p s) (t : (p, s) typed_address) : p contract =
        [%external ("TEST_TO_CONTRACT", t)]

      (** Gets the contract corresponding to an entrypoint of a typed
        address: the contract parameter in the result will be the type of
        the entrypoint, it needs to be annotated, entrypoint string should
        omit the prefix "%", but if passed a string starting with "%", it
        will be removed (and a warning emitted). *)
      let get_entrypoint (type p s q) (s : string) (t : (p, s) typed_address) : q contract =
        let s = if Toplevel.String.length s > 0n then
                  if Toplevel.String.sub 0n 1n s = "%" then
                    let () = IO.eprintln "WARNING: get_entrypoint: automatically removing starting %" in
                    Toplevel.String.sub 1n (abs (Toplevel.String.length s - 1)) s
	                else s
	              else s in
        [%external ("TEST_TO_ENTRYPOINT", s, t)]
    end
    module Address = struct
      let get_balance = get_balance_of_address
      let to_typed_address = cast_address
      let get_storage (type b) (a : address) : b =
        (* we use unit bellow because we don't want inference to force useless annotations *)
        let a : (unit, b) typed_address = to_typed_address a in
        Typed_address.get_storage a
    end
    module Timelock = struct
      let create (b : bytes) (n : nat) : chest * chest_key =
        [%external ("TEST_CREATE_CHEST", b, n)]
      let create_key (c : chest) (n : nat) : chest_key =
        [%external ("TEST_CREATE_CHEST_KEY", c, n)]

      (** display-only-for-cameligo
        The call `verify chest chest_key n` verifies a matching
        between `chest` and `chest_key` (taking into account `n`). *)
      (** display-only-for-jsligo
        The call `verify(chest, chest_key, n)` verifies a matching
        between `chest` and `chest_key` (taking into account `n`). *)
      let verify (c : chest) (ck : chest_key) (n : nat) : bool =
        [%external ("TEST_VERIFY_CHEST", c, ck, n)]
    end
    module Crypto = struct
      let sign (sk : string) (d : bytes) : signature =
        [%external ("TEST_SIGN", sk, d)]
    end
    module Dynamic_entrypoints = struct
      let storage (type p s s2)
        ((_, _, init_opt) : (p, s) module_contract)
        (s:s2)
      =
        type t = [@layout comb] { storage : s2 ; dynamic_entrypoints : dynamic_entrypoints} in
        match init_opt with
        | Some dynamic_entrypoints -> ({storage = s ; dynamic_entrypoints } : t)
        | None -> failwith "Your contract do not have dynamic entrypoints"
    end
    let originate = Originate.contract
    let failwith = Assert.failwith
    include Typed_address
  end
end
