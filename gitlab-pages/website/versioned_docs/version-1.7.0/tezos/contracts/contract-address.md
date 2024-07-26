---
id: contract-address
title: Contract addresses
---

import Syntax from '@theme/Syntax';

In most cases, contracts have addresses that start with `KT1`.
However, they can also be tied to an implicit account and have an address that starts with `kt1`, `kt2` or `kt3`, [depending on the hashing function](https://tezos.gitlab.io/michelson-reference/#type-address).

For information about the type that represents contracts in code, see [Contracts](../contracts).

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
