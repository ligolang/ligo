---
id: current-reference
title: Tezos - Things relating to the current execution context
---

import Syntax from '@theme/Syntax';

# Tezos.balance

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


## Tezos.now

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

> Note that `now` is *deprecated*.

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

> Note that `now` is *deprecated*.

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

> Note that `now` is *deprecated*.

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



## Amount

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


## Sender

Get the address that initiated the current transaction.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) : address is Tezos.sender
```

> Note that `sender` is *deprecated*.

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



## Address

Get the address associated with a value of type `contract`.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : key_hash) : address is block {
  const c : contract (unit) = Tezos.implicit_account (p)
} with Tezos.address(c)
```

> Note that `implicit_account` and `address` are *deprecated*.

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


## Self Address

Get the address of the currently running contract.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : unit) : address is Tezos.self_address
```

> Note that `self_address` is *deprecated*.

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

## Self

Typecast the currently running contract with an entrypoint annotation.
If your are using entrypoints: use "%Bar" for constructor Bar
If you are not using entrypoints: use "%Default"

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

## Implicit Account

Get the default contract associated with an on-chain key-pair. This
contract does not execute code, instead it exists to receive tokens on
behalf of a key's owner.



<Syntax syntax="pascaligo">

```pascaligo
function main (const kh: key_hash) : contract (unit) is
  Tezos.implicit_account (kh)
```

> Note that `implicit_account` is *deprecated*.

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


## Source

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

> Note that `source` is *deprecated*.

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


## Failwith

Cause the contract to fail with an error message.

> ⚠ Using this currently requires in general a type annotation on the
> `failwith` call.



<Syntax syntax="pascaligo">

```pascaligo
function main (const p : int; const s : unit) : list (operation) * unit is
  block {
    if p > 10 then failwith ("Failure.") else skip
  }
  with ((nil : list (operation)), s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p,s : int * unit) = if p > 10 then failwith "Failure."
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = ((p,s) : (int, unit)) =>
  if (p > 10) { failwith ("Failure."); };
```

</Syntax>

