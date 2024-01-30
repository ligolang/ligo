---
id: first-contract
title: First contract
---

import Syntax from '@theme/Syntax';

So far so good, we have learned enough of the LIGO language, we are
confident enough to write out first smart contract.

We will be implementing a counter contract.

## Dry-running a Contract

Testing a contract can be quite easy if we utilise LIGO's built-in dry
run feature. Dry-run works by simulating the main function execution,
as if it were deployed on a real chain. You need to provide the
following:

- `file` - contract to run
- `parameter` - parameter passed to the contract (in a
  theoretical invocation operation)
- `storage` - a mock storage value, as if it were stored on a real chain

Here is a full example:

<Syntax syntax="cameligo">

```cameligo group=first-contract
type storage = int

[@entry]
let main (_action : unit) (store : storage) : operation list * storage =
  ([], store + 1)
```

```shell skip
ligo run dry-run src/basic.mligo unit 42 ./gitlab-pages/docs/advanced/src/first-contract.mligo
// Outputs:
// tuple[   list[]
//          Unit
// ]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=first-contract
type storage = int

@entry
const main = (_action : unit, store : storage) : [list<operation>, storage] =>
  [list([]), store + 1]
```

```shell skip
ligo run dry-run src/basic.mligo unit 42 ./gitlab-pages/docs/advanced/src/first-contract.jsligo
// Outputs:
// tuple[   list[]
//          Unit
// ]
```

</Syntax>

Output of the `dry-run` is the return value of our main function, we
can see the operations emitted (in our case an empty list, and the new
storage value being returned) which in our case is still `Unit`.

## A Counter Contract

Our counter contract will store a single `int` as it's storage, and
will accept an `action` variant in order to re-route our single `main`
function to two entrypoints for `add` (addition) and `sub`
(subtraction).

<Syntax syntax="cameligo">

```cameligo group=counter
type storage = int
type result = operation list * storage

[@entry] let increment (n : int) (store : storage) : result = [], store + n
[@entry] let decrement (n : int) (store : storage) : result = [], store - n

[@view] let v1 (n : int) (store : storage) : int = store + n
```

</Syntax>


<Syntax syntax="jsligo">

```jsligo group=counter

type storage = int;
type result = [list<operation>, storage];

@entry
const increment = (n: int, store: storage): result =>
  [list([]), store + n];

@entry
const decrement = (n: int, store: storage): result =>
  [list([]), store - n];

[@view]
const v1 = (n : int, store : storage) : int => store + n
```

</Syntax>

To dry-run the counter contract, we will provide the `main` function
with a variant parameter of value `Increment (5)` and an initial
storage value of `5`.

```shell
ligo run dry-run ./gitlab-pages/docs/advanced/src/first-contract/counter.mligo "Increment(5)" 5
# tuple[   list[]
#          10
# ]
```

Our contract's storage has been successfully incremented to `10`.

## Deploying and interacting with a contract on a live-chain

In order to deploy the counter contract to a real Tezos network, we'd
have to compile it first, this can be done with the help of the
`compile-contract` CLI command:

```shell
ligo compile contract ./gitlab-pages/docs/advanced/src/first-contract/counter.mligo
```

Command above will output the following Michelson code:

```michelson
{ parameter (or (int %decrement) (int %increment)) ;
  storage int ;
  code { UNPAIR ; IF_LEFT { SWAP ; SUB } { ADD } ; NIL operation ; PAIR } ;
  view "v1" int int { UNPAIR ; ADD } }
```

However in order to originate a Michelson contract on Tezos, we also
need to provide the initial storage value, we can use
`compile-storage` to compile the LIGO representation of the storage to
Michelson.

```shell
ligo compile storage ./gitlab-pages/docs/advanced/src/first-contract/counter.mligo 5
# Outputs: 5
```

In our case the LIGO storage value maps 1:1 to its Michelson
representation, however this will not be the case once the parameter
is of a more complex data type, like a record.

## Invoking a LIGO contract

Same rules apply for parameters, as apply for translating LIGO storage
values to Michelson. We will need to use `compile-parameter` to
compile our `action` variant into Michelson, here's how:

```shell
ligo compile parameter ./gitlab-pages/docs/advanced/src/first-contract/counter.mligo 'Increment(5)'
# Outputs: (Right 5)
```

Now we can use `(Right 5)` which is a Michelson value, to invoke our
contract - e.g., via `tezos-client`

<!-- updated use of entry -->
