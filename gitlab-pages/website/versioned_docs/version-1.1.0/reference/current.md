---
id: current-reference
title: Tezos
description: General operations for Tezos
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val get_balance : unit -> tez
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_balance: (_u: unit) => tez
</SyntaxTitle>

Get the balance for the contract.

<Syntax syntax="cameligo">

```cameligo
let check (p,s : unit * tez) = [], Tezos.get_balance()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
let check = (p: unit, s: tez):[list<operation>, tez] =>
  [list([]), Tezos.get_balance()];
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val get_now : unit -> timestamp
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

<Syntax syntax="cameligo">

```cameligo group=b
let today         = Tezos.get_now ()
let one_day       = 86_400
let in_24_hrs     = today + one_day
let some_date     = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later = some_date + one_day
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

<Syntax syntax="cameligo">

```cameligo group=c
let today     = Tezos.get_now ()
let one_day   = 86_400
let in_24_hrs = today - one_day
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

<Syntax syntax="cameligo">

```cameligo group=c
let not_tomorrow = (Tezos.get_now () = in_24_hrs)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
let not_tomorrow = (Tezos.get_now() == in_24_hrs);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val get_amount : unit -> tez
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_amount: (_u : unit) => tez
</SyntaxTitle>

Get the amount of tez provided by the sender to complete this
transaction.

<Syntax syntax="cameligo">

```cameligo
let threshold (p : unit) = if Tezos.get_amount () = 100tz then 42 else 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
function threshold (p : unit) {
  if (Tezos.get_amount() == 100tez) return 42 else return 0;
};
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val get_sender : unit -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_sender: (_u : unit) => address
</SyntaxTitle>

Get the address that initiated the current transaction.

<Syntax syntax="cameligo">

```cameligo
let check (p : unit) = Tezos.get_sender ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=e
let check = (p : unit) => Tezos.get_sender ();
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val address : 'a contract -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let address: (contract: contract&lt;&apos;a&gt;) => address
</SyntaxTitle>

Get the address associated with a value of type `contract`.

<Syntax syntax="cameligo">

```cameligo
let check (p : key_hash) =
  let c = Tezos.implicit_account p
  in Tezos.address c
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=f
let check = (p : key_hash) => {
  let c = Tezos.implicit_account(p);
  return Tezos.address(c);
};
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val get_self_address : unit -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_self_address: (_u : unit) => address
</SyntaxTitle>

Get the address of the currently running contract.

<Syntax syntax="cameligo">

```cameligo
let check (p : unit) = Tezos.get_self_address ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=g
let check = (p : unit) => Tezos.get_self_address();
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val self : string -> 'a contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let self: (entrypoint: string) => contract&lt;&apos;a&gt;
</SyntaxTitle>

Typecast the currently running contract with an entrypoint annotation.
If you are using entrypoints, use "%bar" for a constructor "Bar". If
you are not using entrypoints: use "%default"

<Syntax syntax="cameligo">

```cameligo
let check (p : unit) = Tezos.self("%default")
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=h
let check = (p: unit) => Tezos.self("%default");
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val implicit_account : key_hash -> 'a contract
</SyntaxTitle>

Get the default contract associated with an on-chain key-pair. This
contract does not execute code, instead it exists to receive tokens on
behalf of a key's owner.

See also: http://tezos.gitlab.io/user/glossary.html#implicit-account

<Syntax syntax="cameligo">

```cameligo
let check (kh : key_hash) = Tezos.implicit_account kh
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=i
let check = (kh: key_hash) => Tezos.implicit_account(kh);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val get_source : unit -> address
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



<Syntax syntax="cameligo">

```cameligo
let check (p : unit) = Tezos.get_source ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=j
let check = (p : unit) => Tezos.get_source();
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val failwith : 'a -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let failwith: (message: &apos;a) => unit
</SyntaxTitle>

[See `failwith`](toplevel.md#failwith)

<SyntaxTitle syntax="cameligo">
val get_chain_id : unit -> chain_id
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_chain_id: (_u : unit) => chain_id
</SyntaxTitle>

Get the identifier of the chain to distinguish between main and test chains.

This is mainly intended to avoid replay attacks between the chains, and can currently
only be used together with `Bytes.pack` and `Bytes.unpack`.

<Syntax syntax="cameligo">

```cameligo
type storage = bytes

[@entry]
let main (_ignore : unit) (store : storage) =
  let packed = Bytes.pack (Tezos.get_chain_id ()) in
  if (store <> packed) then
    (failwith "wrong chain" : (operation list * storage))
  else
    ([], (packed: storage))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=k
type storage = bytes;

@entry
let main = (_ignore: unit, storage: storage) : [list<operation>, storage] => {
  let packed = Bytes.pack(Tezos.get_chain_id());
  if (storage != packed) {
    return failwith("wrong chain") as [list<operation>, storage];
  } else {
    return [list([]), packed];
  };
};
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val transaction : 'param -> mutez -> 'param contract -> operation
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let transaction: (action: &apos;param, amount: mutez, contract: contract&lt;&apos;param&gt;) => operation
</SyntaxTitle>

Transfer `tez` to an account, or run code of another smart contract.

To indicate an account, use `unit` as `param`.

<SyntaxTitle syntax="cameligo">
val create_contract : ('param -> 'storage -> operation list * 'storage) -> key_hash option -> tez -> 'storage -> (operation * address)
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let create_contract = (contract: ('param, 'storage) => (list &lt;operation&gt;, &apos;storage), delegate: option&lt;key_hash&gt;, balance: tez, init: 'storage) => [operation, address]
</SyntaxTitle>

Construct an operation that originates a contract from a function. The
optional argument of type `key_hash` represents a delegate.

<SyntaxTitle syntax="cameligo">
val create_contract_uncurried : ('param * 'storage -> operation list * 'storage) -> key_hash option -> tez -> 'storage -> (operation * address)
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let create_contract_uncurried = (contract: ['param, 'storage] => (list &lt;operation&gt;, &apos;storage), delegate: option&lt;key_hash&gt;, balance: tez, init: 'storage) => [operation, address]
</SyntaxTitle>

Construct an operation that originates a contract from an uncurried function. The
optional argument of type `key_hash` represents a delegate.

<SyntaxTitle syntax="cameligo">
val set_delegate : key_hash option -> operation
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let set_delegate: (delegate: option&lt;key_hash&gt;) => operation
</SyntaxTitle>



Modify the [delegate](http://tezos.gitlab.io/user/glossary.html?highlight=delegate#delegate) of the current contract.

The operation fails when:
- the delegate is the same as current delegate
- the keyhash is not of a registered delegate

Use `None` to withdraw the current delegate.

<SyntaxTitle syntax="cameligo">
val get_contract_opt : address -> 'param contract option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_contract_opt : (a: address) => option&lt;contract&lt;&apos;param&gt;&gt;
</SyntaxTitle>

Get a contract from an address.

When no contract is found or the contract doesn't match the type,
`None` is returned.

<SyntaxTitle syntax="cameligo">
val get_contract_with_error : address -> string -> 'param contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_contract_with_error : (a: address,s: string) => contract&lt;&apos;param&gt;
</SyntaxTitle>

Get a contract from an address.

When no contract is found, fail with the provided string

<SyntaxTitle syntax="cameligo">
val get_entrypoint_opt : string -> address -> 'param contract option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_entrypoint_opt: (entrypoint: string, a: address) => option&lt;contract&lt;&apos;param&gt;&gt;
</SyntaxTitle>

Get a contract from an address and entrypoint.

Entrypoints are written in the form of: `%entrypoint`.

When no contract is found or the contract doesn't match the type,
`None` is returned.

<SyntaxTitle syntax="cameligo">
val get_level : unit -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_level : (_u : unit) => nat
</SyntaxTitle>

Get the current block level.

<SyntaxTitle syntax="cameligo">
val min_block_time : unit -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let min_block_time: unit => nat;
</SyntaxTitle>

Returns the current minimal time between blocks, the value is obtained from the protocol’s minimal_block_delay constant.

<SyntaxTitle syntax="cameligo">
val pairing_check : (bls12_381_g1 * bls12_381_g2) list -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let pairing_check: list&lt;[bls12_381_g1, bls12_381_g2]&gt;) => bool
</SyntaxTitle>

Verify that the product of pairings of the given list of points is equal to 1 in Fq12. Returns true if the list is empty.
Can be used to verify if two pairings P1 and P2 are equal by verifying `P1 * P2^(-1) = 1`.
(extracted from Tezos documentation)

<SyntaxTitle syntax="cameligo">
val never : never -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let never: (never: never) => &apos;a
</SyntaxTitle>

Eliminate a value of the type `never` using the instruction `NEVER`
from Michelson.

<SyntaxTitle syntax="cameligo">
val get_total_voting_power : unit -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_total_voting_power: (_u : unit) => nat
</SyntaxTitle>

Return the total voting power of all contracts. The total voting power coincides with the sum of the rolls count of every contract in the voting listings. The voting listings is calculated at the beginning of every voting period.

<SyntaxTitle syntax="cameligo">
val voting_power : key_hash -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let voting_power: (key_hash:key_hash) => nat
</SyntaxTitle>

Return the voting power of a given contract. The voting power value is the full staking power of the delegate, currently expressed in mutez. Though, developers should not rely on `Tezos.voting_power` to query the staking power of a contract in mutez: the value returned by `Tezos.voting_power` is still of type` nat and it should only be considered relative to `Tezos.total_voting_power`.

## Sapling

Delphi protocol introduced the following sapling types (state and transaction) with N being an int singleton

<Syntax syntax="cameligo">

```cameligo group=sap_t
type st = 8 sapling_state
type tr = 8 sapling_transaction
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sap_t
type st = sapling_state<8>;
type tr = sapling_transaction<8>;
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val sapling_empty_state : 'n sapling_state
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let sapling_empty_state: sapling_state&lt;n&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

```cameligo group=sap_t
let x = Tezos.sapling_empty_state
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sap_t
let x = Tezos.sapling_empty_state ;
```

</Syntax>

Sapling empty state

<SyntaxTitle syntax="cameligo">
val sapling_verify_update : 'a sapling_transaction -> 'a sapling_state -> (bytes * (int * 'a sapling_state)) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let sapling_verify_update: sapling_transaction&lt;'a&gt; => sapling_state&lt;'a&gt; => option&lt;[bytes, [int, sapling_state&lt;'a&gt;]]&gt;
</SyntaxTitle>


Verify sapling update

<Syntax syntax="cameligo">

```cameligo group=sap_t
let f (tr : tr) =
  match Tezos.sapling_verify_update tr x with
    Some (_, x) -> x
  | None -> (failwith "failed" : int * st)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sap_t
let f = (tr : tr) =>
  match (Tezos.sapling_verify_update(tr, x)) {
    when(Some(p)): p[1];
    when(None()): failwith ("failed")
  };
```

</Syntax>

### Linearity

If a contract storage type contains a ticket, you must destructure the parameter-storage pair within the body to preserve storage linearity (e.g. avoid `DUP`-ing storage).
For the same reasons, if tickets are stored in a `map`/`big_map` you must use the new operator `get_and_update` to update your bindings.

<Syntax syntax="cameligo">

```cameligo group=contract_ticket
type storage = (string, int ticket) big_map
type parameter = int
type result = operation list * storage

[@entry]
let main (i : parameter) (store : storage) : result =
  let my_ticket1 = Option.unopt (Tezos.create_ticket i 10n) in
  let _, x = Big_map.get_and_update "hello" (Some my_ticket1) store
  in [], x
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=contract_ticket
type storage = big_map<string, ticket<int>> ;

type parameter = int ;

type result = [list<operation>, storage];

@entry
function main (i: parameter, store : storage): result {
  let my_ticket1 = Option.unopt (Tezos.create_ticket (i, 10n));
  let [_x, ret] = Big_map.get_and_update ("hello", Some(my_ticket1), store);
  return [list([]), ret]
};
```

</Syntax>
