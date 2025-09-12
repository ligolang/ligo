---
id: address
title: Addresses
---

import Syntax from '@theme/Syntax';

An address on the Tezos blockchain is a string whose suffix is the
hash of the public key of a peer. If that peer is a smart contract,
the address is prefixed by the string `"KT1"`; otherwise, it is
associated to an _implicit account_, prefixed by `"kt1"`, `"kt2"` or
`"kt3"`,
[depending on the hashing function](https://tezos.gitlab.io/michelson-reference/#type-address).

There are two ways to define and handle adresses.

### The `address` type

The `address` type in LIGO denotes *a well-formed* Tezos address of
any kind (`tz1`, `tz2`, `tz3`, `tz4`, `KT1` etc.). Currently, such
addresses are created by casting a string to the `address`
type. Beware of failures if the address is invalid. Consider the
following examples.

<Syntax syntax="cameligo">

```cameligo group=address
let my_account : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=address
const my_account: address =
  "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address;
```

</Syntax>

Note that a literal value of type `address` does not entail that it is
the valid address on the chain, only that it is well-formed.

### Self

<Syntax syntax="cameligo">

The call `Tezos.self entrypoint` is the address of the current smart
contract, that is, the smart contract containing the call. For the
address of the smart contract actually *executing* the call, because
it is embedded in a lambda sent to another smart contract, use
`Tezos.get_self_address` instead. The string `entrypoint` is the name
of a valid entrypoint such that `entrypoint` is not `"%default"`, or
the empty string denoting the `"%default"` entrypoint (which is the
root of the smart contract parameter if no `"%default"` entrypoint is
explicitly defined). If the contract does not have the specified
entrypoint, the call results in an type checking error.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.self(entrypoint)` is the address of the current smart
contract, that is, the smart contract containing the call. For the
address of the smart contract actually *executing* the call, because
it is embedded in a lambda sent to another smart contract, use
`Tezos.get_self_address` instead. The string `entrypoint` is the name
of a valid entrypoint such that `entrypoint` is not `"%default"`, or
the empty string denoting the `"%default"` entrypoint (which is the
root of the smart contract parameter if no `"%default"` entrypoint is
explicitly defined). If the contract does not have the specified
entrypoint, the call results in an type checking error.

</Syntax>

Naming convention: if you are using entrypoints, use `"%bar"` to
denote the constructor `"Bar"` of the parameter, in turn corresponding
to the entrypoint function `bar`. If you are not using entrypoints:
use `"%default"`.

<Syntax syntax="cameligo">

```cameligo group=self
let check () = Tezos.self("%default")
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=self
let check = () => Tezos.self("%default");
```

</Syntax>


### Self-Address

Often you want to get the address of the contract being executed. You
can do it with `Tezos.get_self_address`. When used inside a lambda,
that function returns the address of the contract *executing the
lambda*, which can be different from the address of the contract in
which the call is written.

<Syntax syntax="cameligo">

```cameligo group=address
let current_addr : address = Tezos.get_self_address ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=address
const current_addr : address = Tezos.get_self_address();
```

</Syntax>

### Sender

The *sender* is the address of the contract (that is, a smart contract
or an implicit account) that initiated the current internal
transaction. Note that, if transactions have been chained, that
address could be different from the *source*.

<Syntax syntax="cameligo">

```cameligo group=sender
let sender : address = Tezos.get_sender ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sender
const sender: address = Tezos.get_sender();
```

</Syntax>

### Source

The *source* is the address of the implicit account that initiated the
current transaction. If transactions have been chained, that address
is different from the *sender*.

<Syntax syntax="cameligo">

```cameligo group=source
let source : address = Tezos.get_source ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=source
const source: address = Tezos.get_source();
```

</Syntax>


### The `contract` type

In order to handle addresses that denote an originated account on the
chain, we need a value of type `contract`. In fact, it is a type
parameterised by the type of the contract's parameter. Contrary to the
type `address`, there are no literal values of the type `contract`, so
values have to be created by means of predefined functions.

<Syntax syntax="cameligo">

The call `Tezos.implicit_account kh` casts the public key hash `kh`
into the address of its *implicit account*. Note that addresses of
implicit accounts always have the type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.implicit_account(kh)` casts the public key hash `kh`
into the address of its *implicit account*. Note that addresses of
implicit accounts always have the type `contract<unit>`.

</Syntax>
