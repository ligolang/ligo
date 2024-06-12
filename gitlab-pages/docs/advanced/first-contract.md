---
id: first-contract
title: First contract
---

import Syntax from '@theme/Syntax';

So far so good, we have learned enough of the LIGO language, we are
confident enough to write our first smart contract.

We will be implementing a counter contract.

## A counter contract

Our counter contract stores a single `int` as its storage and has two
entrypoints for `add` (addition) and `sub` (subtraction). It also has
a view that provides the value in storage without changing it.

<Syntax syntax="cameligo">

```cameligo group=counter
type storage = int
type result = operation list * storage

[@entry] let add (n : int) (store : storage) : result = [], store + n
[@entry] let sub (n : int) (store : storage) : result = [], store - n

[@view] let get_value (_ : unit) (store : storage) : int = store
```

</Syntax>


<Syntax syntax="jsligo">

```jsligo group=counter
type storage = int;
type result = [list<operation>, storage];

@entry
const add = (n: int, store: storage): result =>
  [[], store + n];

@entry
const sub = (n: int, store: storage): result =>
  [[], store - n];

[@view]
const get_value = (_: unit, store : storage) : int => store
```

</Syntax>

## Dry-running the contract

Testing a contract can be quite easy if we utilise LIGO's built-in dry
run feature. Dry-run works by simulating the contract as if it were
deployed on a real chain. You pass these arguments to the command:

- The contract file to run
- The parameter to pass to the contract, as a LIGO expression
- The value of the contract storage, as a LIGO expression

For example, this command sets the contract storage to 5 and passes 3
to the `add` entrypoint:

```shell
ligo run dry-run ./gitlab-pages/docs/advanced/src/first-contract/counter.mligo "Add(5)" 3
```

The response shows that our contract's storage has been successfully incremented to 8:

```
( LIST_EMPTY() , 8 )
```

The `LIST_EMPTY()` code shows that the contract didn't produce any
other operations.

Note that the function name in the code starts with a lower-case letter but the `run dry-run` command uses entrypoint names that start with an upper-case letter.

For more information about the `run dry-run` command and other ways
to test contracts, see [Testing LIGO](../advanced/testing).

## Compiling contracts and values

To deploy the counter contract to a real Tezos network, which is called
_originating_ the contract, you must compile it to Michelson, the base
language of Tezos smart contracts. To compile a contract, use the
`compile contract` CLI command:

```shell
ligo compile contract ./gitlab-pages/docs/advanced/src/first-contract/counter.mligo
```

The output is the following Michelson code:

```michelson
{ parameter (or (int %sub) (int %add)) ;
  storage int ;
  code { UNPAIR ; IF_LEFT { SWAP ; SUB } { ADD } ; NIL operation ; PAIR } ;
  view "get_value" unit int { CDR } }
```

To originate the contract, you also need the initial storage value
in Michelson. To compile a LIGO expression to Michelson, use the
`compile storage` command, as in this example:

```shell
ligo compile storage ./gitlab-pages/docs/advanced/src/first-contract/counter.mligo 5
```

The output is the integer 5 in Michelson, which is `5`.
In this case the LIGO storage value maps 1:1 to its Michelson
representation. More complex data types like records and maps look
different in Michelson than in LIGO.

You can use these values to originate the contract, such as with the [Octez client](https://tezos.gitlab.io/active/cli-commands.html). This
example assumes that you have saved the compiled contract to a file
named `counter.tz`:

```bash
octez-client originate contract counter \
  transferring 0 from my_account \
  running counter.tz \
  --init 5 --burn-cap 0.5
```

Similarly, you must compile parameters to Michelson to call the
originated contract. For example, this command uses the
`compile parameter` command to compile the parameter to call the `add`
entrypoint:

```shell
ligo compile parameter ./gitlab-pages/docs/advanced/src/first-contract/counter.mligo 'Add(5)'
```

The output is the Michelson value `(Right 5)`. You can use this value
to call the deployed contract, such as with the Octez client:

```bash
octez-client --wait none transfer 0 from my_account to counter \
  --arg '(Right 5)' --burn-cap 0.1
```

This value doesn't mention the entrypoint name because the Michelson
code technically has only one entrypoint, known as the default
entrypoint. The Michelson code branches based on the parameter you pass.

For example, note the first line of the compiled contract:

```michelson
parameter (or (int %sub) (int %add))
```

The annotations `%sub` and `%add` are labels for the two parts of an
option type. If you pass an integer to the left part of the option,
the contract runs the code for the `sub` entrypoint, and if you pass
an integer to the right part of the option, it runs the code for the
`add` entrypoint.

For more information about how entrypoints work on Tezos, see
[Entrypoints](https://docs.tezos.com/smart-contracts/entrypoints) on
docs.tezos.com.

<!-- updated use of entry -->
