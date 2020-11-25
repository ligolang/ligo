---
id: current-reference
title: Tezos
description: General operations for Tezos
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
function balance : tez
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val balance : tez
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let balance: tez
</SyntaxTitle>

Get the balance for the contract.

<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit; const s: tez) : list (operation) * tez is
  ((nil : list (operation)), Tezos.balance)
```

> Note that `balance` and `Current.balance` are *deprecated*. 

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p,s : unit * tez) = ([] : operation list), Tezos.balance
```

> Note that `balance` and `Current.balance` are *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = ((p,s) : (unit, tez)) =>
  ([]: list (operation), Tezos.balance);
```

> Note that `balance` and `Current.balance` are *deprecated*.

</Syntax>


<SyntaxTitle syntax="pascaligo">
function now : timestamp
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val now : timestamp
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let now: timestamp
</SyntaxTitle>

Returns the current time as a [unix timestamp](https://en.wikipedia.org/wiki/Unix_time).

In LIGO, timestamps are type compatible in operations with
integers. This lets you set for instance time constraints for your
smart contracts like this:

### Examples

#### 24 hours from now

<Syntax syntax="pascaligo">

```pascaligo group=b
const today: timestamp = Tezos.now;
const one_day: int = 86_400;
const in_24_hrs: timestamp = today + one_day;
const some_date: timestamp = ("2000-01-01T10:10:10Z" : timestamp);
const one_day_later: timestamp = some_date + one_day;
```

> Note that `now` is *deprecated*. Please use `Tezos.now`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let today: timestamp = Tezos.now
let one_day: int = 86_400
let in_24_hrs: timestamp = today + one_day
let some_date: timestamp = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later: timestamp = some_date + one_day
```

> Note that `Current.time` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let today: timestamp = Tezos.now;
let one_day: int = 86_400;
let in_24_hrs: timestamp = today + one_day;
let some_date: timestamp = ("2000-01-01t10:10:10Z" : timestamp);
let one_day_later: timestamp = some_date + one_day;
```

> Note that `Current.time` is *deprecated*.

</Syntax>


#### 24 hours ago


<Syntax syntax="pascaligo">

```pascaligo group=c
const today: timestamp = Tezos.now;
const one_day: int = 86_400;
const in_24_hrs: timestamp = today - one_day;
```

> Note that `now` is *deprecated*. Please use `Tezos.now`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let today: timestamp = Tezos.now
let one_day: int = 86_400
let in_24_hrs: timestamp = today - one_day
```

> Note that `Current.time` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let today: timestamp = Tezos.now;
let one_day: int = 86_400;
let in_24_hrs: timestamp = today - one_day;
```

> Note that `Current.time` is *deprecated*.

</Syntax>


#### Comparing Timestamps

You can also compare timestamps using the same comparison operators as
for numbers


<Syntax syntax="pascaligo">

```pascaligo group=c
const not_tommorow: bool = (Tezos.now = in_24_hrs)
```

> Note that `now` is *deprecated*. Please use `Tezos.now`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let not_tomorrow: bool = (Tezos.now = in_24_hrs)
```

> Note that `Current.time` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let not_tomorrow: bool = (Tezos.now == in_24_hrs);
```

> Note that `Current.time` is *deprecated*.

</Syntax>



<SyntaxTitle syntax="pascaligo">
function amount : tez
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val amount : tez
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let amount: tez
</SyntaxTitle>

Get the amount of tez provided by the sender to complete this
transaction.



<Syntax syntax="pascaligo">

```pascaligo
function threshold (const p : unit) : int is
  if Tezos.amount = 100tz then 42 else 0
```

> Note that `amount` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let threshold (p : unit) : int = if Tezos.amount = 100tz then 42 else 0
```

> Note that `Current.amount` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let threshold = (p : unit) : int =>
  if (Tezos.amount == 100tz) { 42; } else { 0; };
```

> Note that `Current.amount` is *deprecated*.

</Syntax>


<SyntaxTitle syntax="pascaligo">
function sender : address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val sender : address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let sender: address
</SyntaxTitle>

Get the address that initiated the current transaction.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) : address is Tezos.sender
```

> Note that `sender` is *deprecated*. Please use `Tezos.sender`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p: unit) : address = Tezos.sender
```

> Note that `Current.sender` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p : unit) : address => Tezos.sender;
```

> Note that `Current.sender` is *deprecated*.

</Syntax>



<SyntaxTitle syntax="pascaligo">
function address : contract 'a -> address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val address : 'a contract -> address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let address: contract('a) => address
</SyntaxTitle>

Get the address associated with a value of type `contract`.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : key_hash) : address is block {
  const c : contract (unit) = Tezos.implicit_account (p)
} with Tezos.address(c)
```

> Note that `implicit_account` and `address` are *deprecated*. Please use `Tezos.implicit_account` and `Tezos.address` instead.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : key_hash) =
  let c : unit contract = Tezos.implicit_account p
  in Tezos.address c
```

> Note that `Current.implicit_account` and `Current.address` are
> *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p : key_hash) : address => {
  let c : contract (unit) = Tezos.implicit_account (p);
  Tezos.address (c);
};
```

> Note that `Current.implicit_account` and `Current.address` are
> *deprecated*.

</Syntax>


<SyntaxTitle syntax="pascaligo">
function self_address : address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val self_address : address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let self_address: address
</SyntaxTitle>

Get the address of the currently running contract.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) : address is Tezos.self_address
```

> Note that `self_address` is *deprecated*. Please use `Tezos.self_address`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : unit) : address = Tezos.self_address
```

> Note that `Current.self_address` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p : unit) : address => Tezos.self_address;
```

> Note that `Current.self_address` is *deprecated*.

</Syntax>
<SyntaxTitle syntax="pascaligo">
function self : string -> contract 'a
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val self : string -> 'a contract
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let self: string => contract('a)
</SyntaxTitle>

Typecast the currently running contract with an entrypoint annotation.
If your are using entrypoints: use "%bar" for constructor Bar
If you are not using entrypoints: use "%default"

<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) : contract(unit) is block {
  const c : contract(unit) = Tezos.self("%Default") ;
} with c
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : unit) : unit contract =
  (Tezos.self("%Default") : unit contract)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p: unit) : contract(unit) =>
  (Tezos.self("%Default") : contract(unit));
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function implicit_account : key_hash -> contract 'a
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val implicit_account : key_hash -> 'a contract
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let implicit_account: key_hash => contract('a)
</SyntaxTitle>

Get the default contract associated with an on-chain key-pair. This
contract does not execute code, instead it exists to receive tokens on
behalf of a key's owner.

See also: http://tezos.gitlab.io/user/glossary.html#implicit-account 

<Syntax syntax="pascaligo">

```pascaligo
function main (const kh: key_hash) : contract (unit) is
  Tezos.implicit_account (kh)
```

> Note that `implicit_account` is *deprecated*. Please use `Tezos.implicit_account`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (kh : key_hash) : unit contract = Tezos.implicit_account kh
```

> Note that `Current.implicit_account` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (kh : key_hash): contract (unit) =>
  Tezos.implicit_account (kh);
```

> Note that `Current.implicit_account` is *deprecated*.

</Syntax>


<SyntaxTitle syntax="pascaligo">
function source : address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val source : address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let source: address
</SyntaxTitle>

Get the _originator_ (address) of the current transaction. That is, if
a chain of transactions led to the current execution get the address
that began the chain. Not to be confused with `Tezos.sender`, which
gives the address of the contract or user which directly caused the
current transaction.

> ⚠️ There are a few caveats you should keep in mind before using
> `Tezos.source` over `Tezos.sender`:
>
> 1. `Tezos.source` will never be a contract, so if you want to allow
>    contracts (multisigs etc) to operate your contract, you need to
>    use `Tezos.sender`
> 2. https://vessenes.com/tx-origin-and-ethereum-oh-my/ -- in general
>    it is somewhat unsafe to assume that `Tezos.source` understands
>    everything that is going to happen in a transaction. If
>    `Tezos.source` transfers to a malicious (or sufficiently
>    attackable) contract, that contract might potentially transfer to
>    yours, without `Tezos.source`'s consent. So if you are using
>    `Tezos.source` for authentication, you risk being confused. A
>    good historical example of this is bakers paying out delegation
>    rewards. Naive bakers did (and probably still do) just use
>    tezos-client to transfer to whatever KT1 delegates they had, even
>    if those KT1 were malicious scripts.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p: unit) : address is Tezos.source
```

> Note that `source` is *deprecated*. Please use `Tezos.source`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p : unit) : address = Tezos.source
```

> Note that `Current.source` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = (p : unit) : address => Tezos.source;
```

> Note that `Current.source` is *deprecated*.

</Syntax>


<SyntaxTitle syntax="pascaligo">
function failwith : 'a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
function failwith : 'a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
function failwith: 'a -> unit
</SyntaxTitle>

[See `failwith`](toplevel.md#failwith)


<SyntaxTitle syntax="pascaligo">
function chain_id : chain_id
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val chain_id : chain_id
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let chain_id: chain_id
</SyntaxTitle>

Get the identifier of the chain to distinguish between main and test chains.

This is mainly intended to avoid replay attacks between the chains, and can currently
only be used together with `Bytes.pack` and `Bytes.unpack`.

<Syntax syntax="pascaligo">

```pascaligo
type storage is bytes

function main (const ignore : unit; const storage: storage) : 
  (list(operation) * storage) is block {
  const packed : bytes = Bytes.pack (Tezos.chain_id);
  if (storage =/= packed) then {
   failwith("wrong chain")
  } else
    skip;
} with ((nil: list(operation)), packed)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type storage = bytes

let main ((ignore, storage): (unit * storage)) =
  let packed = Bytes.pack Tezos.chain_id in
  if (storage <> packed) then
    (failwith "wrong chain" : (operation list * storage))
  else
    (([]: operation list), (packed: storage))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type storage = bytes;

let main = ((ignore, storage): (unit, storage)) => {
  let packed = Bytes.pack(Tezos.chain_id);
  if (storage != packed) {
    (failwith("wrong chain"): (list(operation), storage));
  } else {
    ([]: list(operation), packed);
  }
};
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function transaction : 'parameter -> mutez -> contract('parameter) -> operation
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val transaction : 'parameter -> mutez -> 'parameter contract -> operation
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let transaction: 'parameter -> mutez -> contract('parameter) -> operation
</SyntaxTitle>

Transfer `tez` to an account, or run code of another smart contract.

To indicate an account, use `unit` as `parameter`.

<Syntax syntax="pascaligo">
Note that `transaction` is deprecated. Please use `Tezos.transaction` instead.
</Syntax>
<Syntax syntax="cameligo">
Note that `transaction` is deprecated. Please use `Tezos.transaction` instead.
</Syntax>
<Syntax syntax="reasonligo">
Note that `transaction` is deprecated. Please use `Tezos.transaction` instead.
</Syntax>

<SyntaxTitle syntax="pascaligo">
function set_delegate : option(key_hash) -> operation
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val set_delegate : key_hash option -> operation
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let set_delegate: option(key_hash) => operation
</SyntaxTitle>

Modify the [delegate](http://tezos.gitlab.io/user/glossary.html?highlight=delegate#delegate) of the current contract.

The operation fails when:
- the delegate is the same as current delegate
- the keyhash is not of a registered delegate

Use `None` to withdraw the current delegate.

<Syntax syntax="pascaligo">
Note that `set_delegate` is deprecated. Please use `Tezos.set_delegate` 
instead.
</Syntax>
<Syntax syntax="cameligo">
Note that `Operation.set_delegate` is deprecated. Please use 
`Tezos.set_delegate` instead.
</Syntax>
<Syntax syntax="reasonligo">
Note that `Operation.set_delegate` is deprecated. Please use 
`Tezos.set_delegate` instead.
</Syntax>

<SyntaxTitle syntax="pascaligo">
function get_contract_opt : address -> option(contract('parameter))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_contract_opt : address -> 'parameter contract option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_contract_opt : address => option(contract('parameter))
</SyntaxTitle>

Get a contract from an address.

When no contract is found or the contract doesn't match the type,  
`None` is returned.

<Syntax syntax="pascaligo">
Note that `get_contract` and `get_contract_opt` are deprecated. Please use
`Tezos.get_contract_opt` instead.
</Syntax>
<Syntax syntax="cameligo">
Note that `Operation.get_contract` and `Operation.get_contract_opt` are 
deprecated. Please use `Tezos.get_contract_opt` instead.
</Syntax>
<Syntax syntax="reasonligo">
Note that `Operation.get_contract` and `Operation.get_contract_opt` are 
deprecated. Please use `Tezos.get_contract_opt` instead.
</Syntax>

<SyntaxTitle syntax="pascaligo">
function get_entrypoint_opt : string -> address -> option(contract('parameter))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
function get_entrypoint_opt : string -> address -> 'parameter contract option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
function get_entrypoint_opt: (string, address) => option(contract('parameter))
</SyntaxTitle>

Get a contract from an address and entrypoint. 

Entrypoints are written in the form of: `%entrypoint`.

When no contract is found or the contract doesn't match the type,  
`None` is returned.

<Syntax syntax="pascaligo">
Note that `get_entrypoint` and `get_entrypoint_opt` are deprecated. Please use 
`Tezos.get_entrypoint_opt` instead.
</Syntax>

<Syntax syntax="cameligo">
Note that `Operation.get_entrypoint` and `Operation.get_entrypoint_opt` are 
deprecated. Please use `Tezos.get_entrypoint_opt` instead.
</Syntax>

<Syntax syntax="reasonligo">
Note that `Operation.get_entrypoint` and `Operation.get_entrypoint_opt` are 
deprecated. Please use `Tezos.get_entrypoint_opt` instead.
</Syntax>