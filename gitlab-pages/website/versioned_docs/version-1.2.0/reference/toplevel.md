---
id: toplevel
title: Top-Level
description: Available functions at the top level
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

These types and functions are available without any needed prefix.

<SyntaxTitle syntax="cameligo">
type address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type address
</SyntaxTitle>

An untyped address which can refer to a smart contract or account.

<SyntaxTitle syntax="cameligo">
type ('key, 'value) big_map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type big_map &lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The type of a big map from values of type `key` to values
of type `value` is `(key, value) big_map`.

```cameligo group=big_map
type move = int * int
type register = (address, move) big_map
```

</Syntax>

<Syntax syntax="jsligo">

The type of a big map from values of type `key` to
values of type `value` is `big_map<key, value>`.

```jsligo group=big_map
type move = [int, int];
type register = big_map<address, move>;
```

</Syntax>

Be aware that a `big_map` cannot appear inside another `big_map`.

<SyntaxTitle syntax="cameligo">
type bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type bool
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
type bytes
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type bytes
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
type 'param contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type contract&lt;&apos;param&gt;
</SyntaxTitle>

A typed contract.

Use `unit` as `param` to indicate an implicit account.

<SyntaxTitle syntax="cameligo">
type chain_id
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type chain_id
</SyntaxTitle>

The identifier of a chain, used to indicate test or main chains.

<SyntaxTitle syntax="cameligo">
type int
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type int
</SyntaxTitle>

An integer.

The only size limit to integers is gas.

<SyntaxTitle syntax="cameligo">
type key
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type key
</SyntaxTitle>

A public cryptographic key.

<SyntaxTitle syntax="cameligo">
type key_hash
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type key_hash
</SyntaxTitle>

The hash of a public cryptographic key.

<SyntaxTitle syntax="cameligo">
type 't list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type list&lt;&apos;t&gt;
</SyntaxTitle>

A sequence of elements of the same type.

<SyntaxTitle syntax="cameligo">
type ('key, 'value) map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type map &lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The type of a map from values of type `key` to values
of type `value` is `(key, value) map`.

```cameligo group=maps
type move = int * int
type register = (address, move) map
```

</Syntax>

<Syntax syntax="jsligo">

The type of a map from values of type `key` to
values of type `value` is `map <key, value>`.

```jsligo group=maps
type move = [int, int];
type register = map <address, move>;
```

</Syntax>

<SyntaxTitle syntax="cameligo">
type nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type nat
</SyntaxTitle>

A natural number.

The only size limit to natural numbers is gas.

<SyntaxTitle syntax="cameligo">
type operation
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type operation
</SyntaxTitle>

An operation emitted by the contract

<SyntaxTitle syntax="cameligo">
type 'value set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type set&lt;&apos;value&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
type signature
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type signature
</SyntaxTitle>

A cryptographic signature.


<SyntaxTitle syntax="cameligo">
type string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type string
</SyntaxTitle>

A sequence of characters.

<SyntaxTitle syntax="cameligo">
type tez
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type tez
</SyntaxTitle>

A specific type for tokens.

<SyntaxTitle syntax="cameligo">
type timestamp
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type timestamp
</SyntaxTitle>

A date in the real world.

<SyntaxTitle syntax="cameligo">
type unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type unit
</SyntaxTitle>


<SyntaxTitle syntax="cameligo">
val is_nat: int -> nat option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let is_nat: (i: int) => option&lt;nat&gt;
</SyntaxTitle>

Convert an `int` to a `nat` if possible.

<SyntaxTitle syntax="cameligo">
val abs: int -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let abs: (i: int) => nat
</SyntaxTitle>

Cast an `int` to `nat`.

<SyntaxTitle syntax="cameligo">
val int: nat -> int
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let int: (n: nat) => int
</SyntaxTitle>

Cast an `nat` to `int`.

<SyntaxTitle syntax="cameligo">
val unit: unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let unit: unit
</SyntaxTitle>

A helper to create a unit.

<a name="failwith"></a>
<SyntaxTitle syntax="cameligo">
val failwith : 'a -> 'b
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let failwith: (message: &apos;a) => &apos;b
</SyntaxTitle>

Cause the contract to fail with an error message or integer. Other types are
not supported at the moment.

Using this currently requires in general a type annotation on the
`failwith` call.

<Syntax syntax="cameligo">

```cameligo
let main (p : int) (_s : unit) : operation list * unit =
  if p > 10 then failwith "Failure." else [], ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
let main = (p: int, s: unit): [list<operation>, unit] => {
  if (p > 10) { failwith ("Failure."); } else return [list([]), []];
};
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val assert : bool -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let assert: (condition: bool) => unit
</SyntaxTitle>

Check if a certain condition has been met. If not the contract will fail.

<SyntaxTitle syntax="cameligo">
val ediv : int -> int -> (int * nat) option
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val ediv : mutez -> nat -> (mutez * mutez) option
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val ediv : mutez -> mutez -> (nat * mutez) option
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val ediv : nat -> nat -> (nat * nat) option
</SyntaxTitle>


<SyntaxTitle syntax="jsligo">
let ediv: (value: int, divided_by: int) => option&lt;[int, nat]&gt;
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let ediv: (value: mutez, divided_by: nat) => option&lt;[mutez, mutez]&gt;
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let ediv: (value: mutez, divided_by: mutez) => option&lt;[nat, mutez]&gt;
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let ediv: (value: nat, divided_by: nat) => option&lt;[nat, nat]&gt;
</SyntaxTitle>

Compiles to Michelson `EDIV`, one operation to get both the quotient and remainder of a division. `ediv x y` returns None if `y` is zero, otherwise returns `Some (quotient, remainder)` such that `x = (quotient * y) + remainder` and `0 <= remainder < abs(y)`.

<SyntaxTitle syntax="cameligo">
val ignore : 'a -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let ignore: (value: &apos;a) => &apos;unit
</SyntaxTitle>

Ignores a value, it can be an alternative to `_` prefixed variables.

<SyntaxTitle syntax="cameligo">
type 'n sapling_state
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
type 'n sapling_transaction
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
type 'v ticket
</SyntaxTitle>

Edo protocol introduced the following ticket type.
Follow this [wallet example](https://gitlab.com/ligolang/ligo/-/blob/dev/src/test/contracts/ticket_wallet.mligo) for an example of
correct usage (it goes with its [builder](https://gitlab.com/ligolang/ligo/-/blob/dev/src/test/contracts/ticket_builder.mligo)).
This [article](https://medium.com/tezos-israel/tickets-on-edo-simply-explained-c5a411cc27f9) might also be useful.

Note that a variable containing a ticket can only be used once (they are not `DUP`-able).

The ticket type can be defined over a comparable type `'v`.
`'v` being the type of the value used to identify a given ticket.

<Syntax syntax="cameligo">

```cameligo group=ticket_t
type va = int
type my_ticket = va ticket
```

</Syntax>
