---
id: current-reference
title: Tezos
description: General operations for Tezos
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
val get_balance : unit -> tez
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_balance : unit -> tez
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_balance: unit => tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_balance: (_u: unit) => tez
</SyntaxTitle>

Get the balance for the contract.

<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit; const s : tez) : list (operation) * tez is
  (nil, Tezos.get_balance ())
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p,s : unit * tez) = [], Tezos.get_balance()
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = ((p,s) : (unit, tez)) =>
  ([], Tezos.get_balance());
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let main = (p: unit, s: tez):[list<operation>, tez] =>
  [list([]), Tezos.get_balance()];
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
val get_now : unit -> timestamp
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_now : unit -> timestamp
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_now: unit => timestamp
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_now: (_u : unit) => timestamp
</SyntaxTitle>

Returns the current time as a [UNIX timestamp](https://en.wikipedia.org/wiki/Unix_time).

In LIGO, timestamps are type compatible in operations with
integers. This lets you set for instance time constraints for your
smart contracts like this:

### Examples

#### 24 hours from now

<Syntax syntax="pascaligo">

```pascaligo group=b
const today         = Tezos.get_now();
const one_day       = 86_400;
const in_24_hrs     = today + one_day;
const some_date     = ("2000-01-01T10:10:10Z" : timestamp);
const one_day_later = some_date + one_day;
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let today         = Tezos.get_now ()
let one_day       = 86_400
let in_24_hrs     = today + one_day
let some_date     = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later = some_date + one_day
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let today         = Tezos.get_now();
let one_day       = 86_400;
let in_24_hrs     = today + one_day;
let some_date     = ("2000-01-01t10:10:10Z" : timestamp);
let one_day_later = some_date + one_day;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let today         = Tezos.get_now();
let one_day       = 86_400;
let in_24_hrs     = today + one_day;
let some_date     = ("2000-01-01t10:10:10Z" as timestamp);
let one_day_later = some_date + one_day;
```

</Syntax>


#### 24 hours ago


<Syntax syntax="pascaligo">

```pascaligo group=c
const today     = Tezos.get_now()
const one_day   = 86_400
const in_24_hrs = today - one_day
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let today     = Tezos.get_now ()
let one_day   = 86_400
let in_24_hrs = today - one_day
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let today     = Tezos.get_now();
let one_day   = 86_400;
let in_24_hrs = today - one_day;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let today     = Tezos.get_now();
let one_day   = 86_400;
let in_24_hrs = today - one_day;
```

</Syntax>


#### Comparing Timestamps

You can also compare timestamps using the same comparison operators as
for numbers


<Syntax syntax="pascaligo">

```pascaligo group=c
const not_tommorow = (Tezos.get_now() = in_24_hrs)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let not_tomorrow = (Tezos.get_now () = in_24_hrs)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let not_tomorrow = (Tezos.get_now() == in_24_hrs);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let not_tomorrow = (Tezos.get_now() == in_24_hrs);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
val get_amount : unit -> tez
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_amount : unit -> tez
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_amount: unit => tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_amount: (_u : unit) => tez
</SyntaxTitle>

Get the amount of tez provided by the sender to complete this
transaction.



<Syntax syntax="pascaligo">

```pascaligo
function threshold (const p : unit) is
  if Tezos.get_amount() = 100tz then 42 else 0
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let threshold (p : unit) = if Tezos.get_amount () = 100tz then 42 else 0
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let threshold = (p : unit) =>
  if (Tezos.get_amount() == 100tz) { 42; } else { 0; };
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let threshold = (p : unit) => {
  if (Tezos.get_amount() == (100 as tez)) { return 42; } else { return 0; };
};
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val get_sender : unit -> address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_sender : unit -> address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_sender: unit => address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_sender: (_u : unit) => address
</SyntaxTitle>

Get the address that initiated the current transaction.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) is Tezos.get_sender()
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : unit) = Tezos.get_sender ()
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p : unit) => Tezos.get_sender ();
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=e
let main = (p : unit) => Tezos.get_sender ();
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val address&lt;a&gt; : contract (a) -> address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val address : 'a contract -> address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let address: contract('a) => address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let address: (contract: contract&lt;&apos;a&gt;) => address
</SyntaxTitle>

Get the address associated with a value of type `contract`.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : key_hash) is {
  const c = Tezos.implicit_account (p)
} with Tezos.address (c)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : key_hash) =
  let c = Tezos.implicit_account p
  in Tezos.address c
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p : key_hash) => {
  let c = Tezos.implicit_account (p);
  Tezos.address (c);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=f
let main = (p : key_hash) => {
  let c = Tezos.implicit_account(p);
  return Tezos.address(c);
};
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
val get_self_address : unit -> address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_self_address : unit -> address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_self_address: unit => address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_self_address: (_u : unit) => address
</SyntaxTitle>

Get the address of the currently running contract.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) is Tezos.get_self_address()
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : unit) = Tezos.get_self_address ()
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p : unit) => Tezos.get_self_address ();
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=g
let main = (p : unit) => Tezos.get_self_address();
```

</Syntax>
<SyntaxTitle syntax="pascaligo">
val self&lt;a&gt; : string -> contract (a)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val self : string -> 'a contract
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let self: string => contract('a)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let self: (entrypoint: string) => contract&lt;&apos;a&gt;
</SyntaxTitle>

Typecast the currently running contract with an entrypoint annotation.
If your are using entrypoints: use "%bar" for constructor Bar
If you are not using entrypoints: use "%default"

<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) is {
  const c = Tezos.self ("%default");
} with c
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : unit) = Tezos.self("%default")
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p: unit) => Tezos.self("%default");
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=h
let main = (p: unit) => Tezos.self("%default");
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val implicit_account&lt;a&gt; : key_hash -> contract (a)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val implicit_account : key_hash -> 'a contract
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let implicit_account: key_hash => contract('a)
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let implicit_account: (hash: key_hash) => contract&lt;'a&gt;
</SyntaxTitle>

Get the default contract associated with an on-chain key-pair. This
contract does not execute code, instead it exists to receive tokens on
behalf of a key's owner.

See also: http://tezos.gitlab.io/user/glossary.html#implicit-account

<Syntax syntax="pascaligo">

```pascaligo
function main (const kh : key_hash) is
  Tezos.implicit_account (kh)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (kh : key_hash) = Tezos.implicit_account kh
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (kh : key_hash) => Tezos.implicit_account (kh);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=i
let main = (kh: key_hash) => Tezos.implicit_account(kh);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
val get_source : unit -> address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_source : unit -> address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_source: unit => address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_source: (_u : unit) => address
</SyntaxTitle>

Get the _originator_ (address) of the current transaction. That is, if
a chain of transactions led to the current execution get the address
that began the chain. Not to be confused with `Tezos.get_sender`, which
gives the address of the contract or user which directly caused the
current transaction.

> ⚠️ There are a few caveats you should keep in mind before using
> `Tezos.get_source` over `Tezos.get_sender`:
>
> 1. `Tezos.get_source` will never be a contract, so if you want to allow
>    contracts (multisigs etc) to operate your contract, you need to
>    use `Tezos.get_sender`
> 2. https://vessenes.com/tx-origin-and-ethereum-oh-my/ -- in general
>    it is somewhat unsafe to assume that `Tezos.get_source` understands
>    everything that is going to happen in a transaction. If
>    `Tezos.get_source` transfers to a malicious (or sufficiently
>    attackable) contract, that contract might potentially transfer to
>    yours, without `Tezos.get_source`'s consent. So if you are using
>    `Tezos.get_source` for authentication, you risk being confused. A
>    good historical example of this is bakers paying out delegation
>    rewards. Naive bakers did (and probably still do) just use
>    tezos-client to transfer to whatever KT1 delegates they had, even
>    if those KT1 were malicious scripts.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) is Tezos.get_source()
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : unit) = Tezos.get_source ()
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p : unit) => Tezos.get_source ();
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=j
let main = (p : unit) => Tezos.get_source();
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
val failwith&lt;a&gt; : a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val failwith : 'a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let failwith: 'a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let failwith: (message: &apos;a) => unit
</SyntaxTitle>

[See `failwith`](toplevel.md#failwith)


<SyntaxTitle syntax="pascaligo">
val get_chain_id : unit chain_id
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_chain_id : unit -> chain_id
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_chain_id: unit => chain_id
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_chain_id: (_u : unit) => chain_id
</SyntaxTitle>

Get the identifier of the chain to distinguish between main and test chains.

This is mainly intended to avoid replay attacks between the chains, and can currently
only be used together with `Bytes.pack` and `Bytes.unpack`.

<Syntax syntax="pascaligo">

```pascaligo
type storage is bytes

function main (const ignore : unit; const storage: storage) : list (operation) * storage is {
  const packed = Bytes.pack (Tezos.get_chain_id());
  if storage =/= packed then failwith ("wrong chain");
} with (nil, packed)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type storage = bytes

let main ((ignore, storage): (unit * storage)) =
  let packed = Bytes.pack (Tezos.get_chain_id ()) in
  if (storage <> packed) then
    (failwith "wrong chain" : (operation list * storage))
  else
    ([], (packed: storage))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type storage = bytes;

let main = ((ignore, storage): (unit, storage)) => {
  let packed = Bytes.pack(Tezos.get_chain_id());
  if (storage != packed) {
    (failwith("wrong chain"): (list(operation), storage));
  } else {
    ([], packed);
  }
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=k
type storage = bytes;

let main = (ignore: unit, storage: storage):[list<operation>, storage] => {
  let packed = Bytes.pack(Tezos.get_chain_id());
  if (storage != packed) {
    return failwith("wrong chain") as [list<operation>, storage];
  } else {
    return [list([]), packed];
  };
};
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val transaction&lt;param&gt; : param -> mutez -> contract (param) -> operation
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val transaction : 'param -> mutez -> 'param contract -> operation
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let transaction: ('param, mutez , contract('param)) => operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transaction: (action: &apos;param, amount: mutez, contract: contract&lt;&apos;param&gt;) => operation
</SyntaxTitle>

Transfer `tez` to an account, or run code of another smart contract.

To indicate an account, use `unit` as `param`.

<SyntaxTitle syntax="pascaligo">
val create_contract &lt;param, storage&gt; : (param * storage -> list (operation) * storage) -> key_hash option -> tez -> storage -> (operation * address)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val create_contract : ('param * 'storage -> operation list * 'storage) -> key_hash option -> tez -> 'storage -> (operation * address)
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let create_contract : (('param, 'storage) -> (list(operation), 'storage)) => option(key_hash) => tez => 'storage => (operation, address)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create_contract = (contract: ('param, 'storage) => (list &lt;operation&gt;, &apos;storage), delegate: option&lt;key_hash&gt;, balance: tez, init: 'storage) => [operation, address]
</SyntaxTitle>

Construct an operation that originates a contract from a function. The
optional argument of type `key_hash` represents a delegate.

<SyntaxTitle syntax="pascaligo">
val set_delegate : option (key_hash) -> operation
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val set_delegate : key_hash option -> operation
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let set_delegate: option(key_hash) => operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set_delegate: (delegate: option&lt;key_hash&gt;) => operation
</SyntaxTitle>



Modify the [delegate](http://tezos.gitlab.io/user/glossary.html?highlight=delegate#delegate) of the current contract.

The operation fails when:
- the delegate is the same as current delegate
- the keyhash is not of a registered delegate

Use `None` to withdraw the current delegate.

<SyntaxTitle syntax="pascaligo">
val get_contract_opt&lt;param&gt; : address -> option (contract (param))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_contract_opt : address -> 'param contract option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_contract_opt : address => option(contract('param))
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_contract_opt : (a: address) => option&lt;contract&lt;&apos;param&gt;&gt;
</SyntaxTitle>

Get a contract from an address.

When no contract is found or the contract doesn't match the type,
`None` is returned.

<SyntaxTitle syntax="pascaligo">
val get_contract_with_error&lt;param&gt; : address -> string -> contract (param)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_contract_with_error : address -> string -> 'param contract
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_contract_with_error : address => string => contract('param)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_contract_with_error : (a: address,s: string) => contract&lt;&apos;param&gt;&gt;
</SyntaxTitle>

Get a contract from an address.

When no contract is found, fail with the provided string

<SyntaxTitle syntax="pascaligo">
val get_entrypoint_opt&lt;param&gt; : string -> address -> option (contract (param))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_entrypoint_opt : string -> address -> 'param contract option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_entrypoint_opt: (string, address) => option(contract('param))
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_entrypoint_opt: (entrypoint: string, a: address) => option&lt;contract&lt;&apos;param&gt;&gt;
</SyntaxTitle>

Get a contract from an address and entrypoint.

Entrypoints are written in the form of: `%entrypoint`.

When no contract is found or the contract doesn't match the type,
`None` is returned.

<SyntaxTitle syntax="pascaligo">
val get_level : unit -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_level : unit -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_level : unit => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_level : (_u : unit) => nat
</SyntaxTitle>

Get the current block level.

<SyntaxTitle syntax="pascaligo">
val min_block_time : unit -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val min_block_time : unit -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let min_block_time: unit => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let min_block_time: unit => nat;
</SyntaxTitle>

Returns the current minimal time between blocks, the value is obtained from the protocol’s minimal_block_delay constant.

<SyntaxTitle syntax="pascaligo">
val pairing_check : list (bls12_381_g1 * bls12_381_g2) -> bool
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val pairing_check : (bls12_381_g1 * bls12_381_g2) list -> bool
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let pairing_check: list(bls12_381_g1 , bls12_381_g2) => bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let pairing_check: list&lt;[bls12_381_g1, bls12_381_g2]&gt;) => bool
</SyntaxTitle>

Verify that the product of pairings of the given list of points is equal to 1 in Fq12. Returns true if the list is empty.
Can be used to verify if two pairings P1 and P2 are equal by verifying `P1 * P2^(-1) = 1`.
(extracted from Tezos documentation)

<SyntaxTitle syntax="pascaligo">
val never&lt;a&gt; : never -> a
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val never : never -> 'a
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let never: never => 'a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let never: (never: never) => &apos;a
</SyntaxTitle>

Eliminate a value of the type `never` using the instruction `NEVER`
from Michelson.

<SyntaxTitle syntax="pascaligo">
function get_total_voting_power : unit -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_total_voting_power : unit -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_total_voting_power: unit => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_total_voting_power: (_u : unit) => nat
</SyntaxTitle>

Return the total voting power of all contracts. The total voting power coincides with the sum of the rolls count of every contract in the voting listings. The voting listings is calculated at the beginning of every voting period.


<SyntaxTitle syntax="pascaligo">
function voting_power : key_hash -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val voting_power : key_hash -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let voting_power: key_hash =>nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let voting_power: (key_hash:key_hash) => nat
</SyntaxTitle>

Return the voting power of a given contract. The voting power value is the full staking power of the delegate, currently expressed in mutez. Though, developers should not rely on `Tezos.voting_power` to query the staking power of a contract in mutez: the value returned by `Tezos.voting_power` is still of type` nat and it should only be considered relative to `Tezos.total_voting_power`.  

## Sapling

Delphi protocol introduced the following sapling types (state and transaction) with N being an int singleton

<Syntax syntax="pascaligo">

```pascaligo group=sap_t
type st is sapling_state (8)
type tr is sapling_transaction (8)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sap_t
type st = 8 sapling_state
type tr = 8 sapling_transaction
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sap_t
type st = sapling_state(8);
type tr = sapling_transaction(8);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=sap_t
type st = sapling_state<8>;
type tr = sapling_transaction<8>;
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val sapling_empty_state&lt;n&gt; : sapling_state (n)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val sapling_empty_state : 'n sapling_state
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let sapling_empty_state: sapling_state('a)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sapling_empty_state: sapling_state&lt;n&gt;
</SyntaxTitle>

<Syntax syntax="pascaligo">

```pascaligo group=sap_t
const x = Tezos.sapling_empty_state
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sap_t
let x = Tezos.sapling_empty_state
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sap_t
let x = Tezos.sapling_empty_state ;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=sap_t
let x = Tezos.sapling_empty_state ;
```

</Syntax>

Sapling empty state

<SyntaxTitle syntax="pascaligo">
val sapling_verify_update&lt;a&gt; : sapling_transaction (a) -> sapling_state (a) -> option (bytes * (int * sapling_state (a)))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val sapling_verify_update : 'a sapling_transaction -> 'a sapling_state -> (bytes * (int * 'a sapling_state)) option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let sapling_verify_update: sapling_transaction('a) => sapling_state('a) => option(bytes, (int, sapling_state('a)))
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sapling_verify_update: sapling_transaction&lt;'a&gt; => sapling_state&lt;'a&gt; => option&lt;[bytes, [int, sapling_state&lt;'a&gt;]]&gt;
</SyntaxTitle>


Verify sapling update

<Syntax syntax="pascaligo">

```pascaligo group=sap_t
function f (const tr : tr) is
  case Tezos.sapling_verify_update (tr, x) of [
    Some (_, x) -> x
  | None -> failwith ("failed")
  ]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sap_t
let f (tr : tr) =
  match Tezos.sapling_verify_update tr x with
    Some (_, x) -> x
  | None -> (failwith "failed" : int * st)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sap_t
let f = (tr : tr) =>
  switch (Tezos.sapling_verify_update (tr, x)) {
    | Some (_, x) => x
    | None => failwith ("failed")
  }
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=sap_t
let f = (tr : tr) =>
  match (Tezos.sapling_verify_update(tr, x), {
    Some: p => p[1],
    None: () => failwith ("failed")
  });
```

</Syntax>

## Tickets

<SyntaxTitle syntax="pascaligo">
val create_ticket&lt;value&gt; : value -> nat -> option (ticket (value))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val create_ticket : 'value -> nat -> ('value ticket) option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let create_ticket : 'value => nat => option(ticket('value))
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create_ticket: 'value => nat => option&lt;ticket&lt;'value&gt;&gt;
</SyntaxTitle>


To create a ticket, the value and the amount of tickets to be created needs to be provided.
The ticket will also contain the contract address it originated from (which corresponds to `Tezos.self`).
The resulting value is `None` if the amount is zero.

<Syntax syntax="pascaligo">

```pascaligo group=manip_ticket
const my_ticket1 = Option.unopt (Tezos.create_ticket (1, 10n))
const my_ticket2 = Option.unopt (Tezos.create_ticket ("one", 10n))
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=manip_ticket
let my_ticket1 = Option.unopt (Tezos.create_ticket 1 10n)
let my_ticket2 = Option.unopt (Tezos.create_ticket "one" 10n)
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo group=manip_ticket
let my_ticket1 = Option.unopt(Tezos.create_ticket(1, 10n));
let my_ticket2 = Option.unopt(Tezos.create_ticket("one", 10n));
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=manip_ticket
let my_ticket1 = Option.unopt(Tezos.create_ticket(1, 10 as nat));
let my_ticket2 = Option.unopt(Tezos.create_ticket("one", 10 as nat));
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
val read_ticket&lt;value&gt; : ticket (value) -> (address * (value * nat)) * ticket (value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val read_ticket : 'value ticket -> (address * ('value * nat)) * 'value ticket
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let read_ticket : ticket('value) => ((address, ('value , nat)) , ticket('value))
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let read_ticket: ticket&lt;'value&gt; => &lt;&lt;address, &lt;'value , nat&gt;&gt; , ticket&lt;'value&gt;&gt;
</SyntaxTitle>


Reading a ticket will return a tuple with the ticket address, the value and the same ticket for later use.
A ticket is only consumed when it is dropped (e.g. `DROP`-ed from the Michelson stack) so if the returned ticket isn't stored in some form by your contract, it will be fully consumed.

<Syntax syntax="pascaligo">

To read the content of a ticket, you need to use pattern matching

```pascaligo group=manip_ticket
const v =
  case Tezos.read_ticket (my_ticket1) of [
    ((_addr, (payload, _amt)), _ticket) -> payload
  ]
```

</Syntax>

<Syntax syntax="cameligo">

To read the content of a ticket, you can either use tuple
destructuring or pattern matching:

```cameligo group=manip_ticket
let v =
  let (_addr, (payload, _amt)), _ticket = Tezos.read_ticket my_ticket1
  in payload
```

</Syntax>
<Syntax syntax="reasonligo">

To read the content of a ticket, you need to use tuple destructuring:

```reasonligo group=manip_ticket
let v2 =
  let ((_addr, (v, _amt)), _ticket) = Tezos.read_ticket (my_ticket2);
  v;
```

</Syntax>
<Syntax syntax="jsligo">

To read the content of a ticket, you need to use tuple destructuring:

```jsligo group=manip_ticket
let v2 = (_: unit) => {
  let [[addr, [v, amt]], ticket] = Tezos.read_ticket (my_ticket2);
  return v;
}
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val split_ticket&lt;value&gt; : ticket (value) -> nat * nat -> option (ticket (value) * ticket (value))
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val split_ticket : 'value ticket -> nat * nat -> ('value ticket * 'value ticket) option
</SyntaxTitle>

<SyntaxTitle syntax="reasonligo">
let split_ticket : ticket('value) => (nat , nat) => option ((ticket('value), ticket('value)))
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let split_ticket: ticket&lt;'value&gt; => &lt;nat , nat&gt; => option &lt;&lt;ticket&lt;'value&gt;, ticket&lt;'value&gt;&gt;&gt;
</SyntaxTitle>

To partially use/consume a ticket, you have to split it.
Provided a ticket and two amounts, two new tickets will be returned to you if, and only if, the sum equals to the amount of the original ticket.

<Syntax syntax="pascaligo">

```pascaligo group=manip_ticket
const x =
  case Tezos.split_ticket (my_ticket1, (6n, 4n)) of [
    None -> failwith ("amt_a + amt_v =/= amt")
  | Some (split_tickets) -> split_tickets
  ]
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=manip_ticket
let ta, tb =
  match Tezos.split_ticket my_ticket1 (6n, 4n) with
    None -> failwith "amt_a + amt_v <> amt"
  | Some split_tickets -> split_tickets
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo group=manip_ticket
let (ta,tb) =
  switch (Tezos.split_ticket(my_ticket1, (6n, 4n))) {
  | None => failwith("amt_a + amt_v != amt")
  | Some split_tickets => split_tickets
  } ;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=manip_ticket
let [ta, tb] =
  match(Tezos.split_ticket(my_ticket1, [6 as nat, 4 as nat]), {
    None: () => failwith("amt_a + amt_v != amt"),
    Some: split_tickets => split_tickets
  });
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val join_tickets&lt;value&gt; : ticket (value) * ticket (value) -> option (ticket (value))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val join_tickets : 'value ticket * 'value ticket -> ('value ticket) option
</SyntaxTitle>

<SyntaxTitle syntax="reasonligo">
let join_tickets : (ticket('value), ticket('value)) => option (ticket('value))
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let join_tickets = &lt;ticket&lt;'value&gt;, ticket&lt;'value&gt;&gt; => option &lt;ticket&lt;'value&gt;&gt;
</SyntaxTitle>

To add two tickets, you have to join them. This works as the inverse
of `Tezos.split_ticket`.  Provided two tickets with the same ticketer
and content, they are deleted and a new ticket will be returned with
an amount equal to the sum of the amounts of the input tickets.

<Syntax syntax="pascaligo">

```pascaligo group=manip_ticket
const tc = {
  const ta = Option.unopt (Tezos.create_ticket (1, 10n));
  const tb = Option.unopt (Tezos.create_ticket (1, 5n))
} with Tezos.join_tickets ((ta, tb))
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=manip_ticket
let tc : int ticket option =
  let ta = Option.unopt (Tezos.create_ticket 1 10n) in
  let tb = Option.unopt (Tezos.create_ticket 1 5n) in
  Tezos.join_tickets (ta, tb)
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo group=manip_ticket
let tc =
  let ta = Option.unopt(Tezos.create_ticket(1, 10n));
  let tb = Option.unopt(Tezos.create_ticket(1, 5n));
  Tezos.join_tickets((ta, tb));
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=manip_ticket2
let ta = Option.unopt(Tezos.create_ticket(1, 10 as nat));
let tb = Option.unopt(Tezos.create_ticket(1, 5 as nat));
let tc = Tezos.join_tickets([ta, tb]);
```

</Syntax>

### Linearity

If a contract storage type contains a ticket, you must destructure the parameter-storage pair within the body to preserve storage linearity (e.g. avoid `DUP`-ing storage).
For the same reasons, if tickets are stored in a `map`/`big_map` you must use the new operator `get_and_update` to update your bindings.

<Syntax syntax="pascaligo">

```pascaligo group=contract_ticket
type storage is big_map (string, ticket (int))
type parameter is int
type return is list (operation) * storage

function main (const i : parameter ; const store : storage) : return is {
  const my_ticket1 = Option.unopt (Tezos.create_ticket (i, 10n));
  const res = Big_map.get_and_update ("hello", (Some (my_ticket1)), store);
  var res := (nil, store);
  case res of [(t,x) -> res := (nil, x)]
} with res
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=contract_ticket
type storage = (string, int ticket) big_map
type parameter = int
type return = operation list * storage

let main (x : parameter * storage) : return =
  let i, store = x in
  let my_ticket1 = Option.unopt (Tezos.create_ticket i 10n) in
  let _, x = Big_map.get_and_update "hello" (Some my_ticket1) store
  in [], x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=contract_ticket
type storage = big_map (string, ticket(int)) ;

type parameter = int ;

type return = (list (operation), storage);

let main = (x : (parameter , storage)) : return => {
  let (i,store) = x ;
  let my_ticket1 = Option.unopt (Tezos.create_ticket (i, 10n)) ;
  let (_,x) = Big_map.get_and_update ("hello", Some(my_ticket1), store) ;
  ([], x)
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=contract_ticket
type storage = big_map<string, ticket<int>> ;

type parameter = int ;

type return_ = [list<operation>, storage];

let main = (x: [parameter, storage]): return_ => {
  let [i, store] = x ;
  let my_ticket1 = Option.unopt (Tezos.create_ticket (i, 10 as nat));
  let [_, x] = Big_map.get_and_update ("hello", Some(my_ticket1), store);
  return [list([]), x]
};
```

</Syntax>


## On Chain Views

<SyntaxTitle syntax="pascaligo">
val call_view&lt;arg,reg&gt; : string -> arg -> address -> option (ret)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val call_view : string -> 'arg -> address -> 'ret option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let call_view : string => 'arg => address => option ('ret)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let call_view : string => 'arg => address => option &lt;&apos;ret&gt;
</SyntaxTitle>

The primitive `Tezos.call_view` will allow you to call another contract view and get its result by providing the view name; the contract address and the parameter of the view. If the address is nonexistent; the name does not match of of the contract
view or the parameter type do not match, `Tezos.call_view` will return `None`.

## Global Constants

<SyntaxTitle syntax="pascaligo">
function constant: string -> 'a
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val constant : string -> 'a
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let constant : string => 'a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let constant : string => 'a
</SyntaxTitle>

The new primitive `Tezos.constant` allows you to use a predefined constant already registered on chain.
It accepts a hash in the form of a string and will require a type annotation.

## Events

<SyntaxTitle syntax="pascaligo">
val emit&lt;a&gt; :  string -> a -> operation
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val emit : string -> 'a -> operation
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let emit: string => 'a => operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let emit: string => &apos;a => operation
</SyntaxTitle>

Build an event operation. To actually emit an event, this operation must be returned the same way as other operations (origination / transfer ..)
