---
id: testing
title: Testing a contract
---

import Syntax from '@theme/Syntax';

Once we have written our contracts, or as we are writing them, it is
good to check that they meet our expectations, that they do what they
are supposed to do.

There are multiple ways of doing this, but in this section we limit
ourselves to *testing*. Testing involves the execution of our contract
(or part of it) to check if certain property holds, and it can be
manual or automated.

When we want to test a contract written in LIGO, we have two options:

* we compile our code to Michelson and test the compiled code

* we use LIGO specific tools to test our LIGO code

## Testing Michelson code

There are multiple frameworks for testing Michelson contracts, we will
not get into details, but here is a list of tutorials showing how to
test contracts in Michelson:

* [PyTezos](https://baking-bad.org/blog/2019/09/16/testing-michelson-tezos-contracts-with-pytezos-library/)

* [Cleveland](https://gitlab.com/morley-framework/morley/-/blob/9455cd384b2ab897fb7b31822abca3730a4ad08b/code/cleveland/testingEDSL.md)

Another alternative is to use Tezos's binary `tezos-client`
directly. There's a new
[mockup](https://tezos.gitlab.io/user/mockup.html) mode which is does
not need a Tezos node to be running (albeit this is less similar to
mainnet than running a Tezos sandboxed node).

### Testing with `tezos-client`'s mockup

We show the main steps that need to be done to use the mockup mode to
test our LIGO contracts. As a first step, we need to compile our LIGO
contract to Michelson code. Suppose we write the following simple
contract:

<Syntax syntax="pascaligo">

```pascaligo
// This is mockup_testme.ligo
type storage is string

type parameter is
  Append of string

type return is list (operation) * storage

function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Append (s) -> store ^ s
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
// This is mockup_testme.mligo
type storage = string

type parameter =
  Append of string

type return = operation list * storage

let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Append (s) -> store ^ s)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
// This is mockup_testme.religo
type storage = string;

type parameter =
  Append (string)

type return = (list (operation), storage);

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Append (s) => store ++ s
  }))
};
```

</Syntax>

To obtain Michelson code from it, we run the LIGO compiler:

<Syntax syntax="pascaligo">

```shell
ligo compile-contract gitlab-pages/docs/advanced/src/testing/mockup_testme.ligo main
// Outputs:
// { parameter string ;
//   storage string ;
//   code { UNPAIR ; SWAP ; CONCAT ; NIL operation ; PAIR } }
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile-contract gitlab-pages/docs/advanced/src/testing/mockup_testme.mligo main
// Outputs:
// { parameter string ;
//   storage string ;
//   code { DUP ; CAR ; SWAP ; CDR ; CONCAT ; NIL operation ; PAIR } }
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile-contract gitlab-pages/docs/advanced/src/testing/mockup_testme.religo main
// Outputs:
// { parameter string ;
//   storage string ;
//   code { DUP ; CAR ; SWAP ; CDR ; CONCAT ; NIL operation ; PAIR } }
```

</Syntax>

Instead of outputting the resulted compiled code in the screen, we can
tell LIGO to write it in a file called `mockup_testme.tz`:

<Syntax syntax="pascaligo">

```shell
ligo compile-contract gitlab-pages/docs/advanced/src/testing/mockup_testme.ligo main --output mockup_testme.tz
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile-contract gitlab-pages/docs/advanced/src/testing/mockup_testme.mligo main --output mockup_testme.tz
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile-contract gitlab-pages/docs/advanced/src/testing/mockup_testme.religo main --output mockup_testme.tz
```

</Syntax>


Now it is time to test this Michelson code we obtained: we want to
execute it using the mockup mode.

Before anything, make sure you have installed `tezos-client`, a simple
way to do so is by using opam (`opam install tezos-client`).

We can list all the protocols available using `tezos-client list
mockup protocols`. In this example, we will use Edo for testing, so
the command we use for creating a mockup instance on the directory
`/tmp/mockup/` is:

```shell
tezos-client \
  --protocol PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq \
  --base-dir /tmp/mockup \
  --mode mockup \
  create mockup
```

This command returns a list of Tezos addresses that we can use with
the client in subsequent commands. As recommended in the Tezos
documentation, we can add a shell alias to avoid mistakes:

```shell
alias mockup-client='tezos-client --mode mockup --base-dir /tmp/mockup'
```

We can list the addresses returned above by running:
```shell
mockup-client list known addresses
// Outputs:
// bootstrap5: tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv (unencrypted sk known)
// bootstrap4: tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv (unencrypted sk known)
// bootstrap3: tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU (unencrypted sk known)
// bootstrap2: tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN (unencrypted sk known)
// bootstrap1: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx (unencrypted sk known)
```

We are now ready to originate (or "deploy") the contract on our mockup
Tezos:

```shell
mockup-client originate contract mockup_testme \
              transferring 0 from bootstrap1 \
              running "`cat mockup_testme.tz`" \
              --init \"foo\" --burn-cap 0.1
```

The `--init` argument (`"foo"`) is the initial storage for our
deployed contract. In case we had a more complex storage, we could
have used LIGO's `compile-storage` sub-command to compile a LIGO
expression to a Michelson storage.

Now it is time to test! The property we want to check is that if we
execute `Append ("bar")` on our contract with storage `"foo"`, then
the contract updates its storage to `"foobar"`.

As a first sanity check, we can confirm that the storage is currently `"foo"`:

```shell
mockup-client get contract storage for mockup_testme
// Outputs:
// "foo"
```

Then, we execute a call to our contract with parameter `Append
("bar")`. To do so, we first compile the parameter as follows:

<Syntax syntax="pascaligo">

```shell
ligo compile-parameter gitlab-pages/docs/advanced/src/testing/mockup_testme.ligo main "Append (\"bar\")"
// Outputs:
// "bar"
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile-parameter gitlab-pages/docs/advanced/src/testing/mockup_testme.mligo main "Append (\"bar\")"
// Outputs:
// "bar"
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile-parameter gitlab-pages/docs/advanced/src/testing/mockup_testme.religo main "Append (\"bar\")"
// Outputs:
// "bar"
```

</Syntax>

So our parameter is simply the string (notice that the constructor
`Append` was removed). We execute a call to the contract with this
compiled parameter as follows:

```shell
mockup-client transfer 0 from bootstrap2 \
              to mockup_testme \
              --arg \"bar\" --burn-cap 0.01
```

We have chosen `bootstrap2` as the origin of this call (for no
particular reason, any address could do).

We can finally check that that our property holds: the storage is now
"foobar":

```shell
mockup-client get contract storage for mockup_testme
// Outputs:
// "foobar"
```

Good! Our contract passed the test successfully!

## Testing LIGO code

The LIGO command-line interpreter provides sub-commands to test
directly your LIGO code. The three main sub-commands we currently
support are:

* `interpret`

* `test`

* `dry-run`

We will show how to use the first two, while an example on how to use
the third one was already explained in the
[here](first-contract.md#dry-running-a-contract).

### Testing with `interpret`

The sub-command `interpret` allows to interpret an expression in a
context initialised by a source file. The interpretation is done using
Michelson's interpreter.

Let's see how it works on an example. Suppose we write the following
contract which we want to test.

<Syntax syntax="pascaligo">

```pascaligo
// This is testme.ligo
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

// Two entrypoints
function add (const store : storage; const delta : int) : storage is
  store + delta
function sub (const store : storage; const delta : int) : storage is
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
// This is testme.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints
let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
// This is testme.religo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

// Two entrypoints
let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
};
```

</Syntax>

This contract keeps an integer as storage, and has three entry-points:
one for incrementing the storage, one for decrementing the storage,
and one for resetting the storage to `0`.

As a simple property, we check whether starting with an storage of
`10`, if we execute the entry-point for incrementing `32`, then we get
a resulting storage of `42`. For checking it, we can interpret the
`main` function:

<Syntax syntax="pascaligo">

```shell
ligo interpret --init-file gitlab-pages/docs/advanced/src/testing/testme.ligo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo interpret --init-file gitlab-pages/docs/advanced/src/testing/testme.mligo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo interpret --init-file gitlab-pages/docs/advanced/src/testing/testme.religo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>

With the argument `--init-file` we pass the contract we want to test,
and the sub-command requires also the expression to evaluate in that
context, in this case, a call to our contract (`main`) with parameter
`Increment (32)` and storage `10`. As a result, we can check that the
resulting storage is `42` (the second component of the pair), and
there are no further operations to execute (the first component).

We can tune certain parameters of the execution by passing them as
arguments:

```
--amount=AMOUNT (absent=0)
    AMOUNT is the amount the Michelson interpreter will use for the
    transaction.
--balance=BALANCE (absent=0)
    BALANCE is the balance the Michelson interpreter will use for the
    contract balance.
--now=NOW
    NOW is the NOW value the Michelson interpreter will use
    (e.g. '2000-01-01T10:10:10Z')
--sender=SENDER
    SENDER is the sender the Michelson interpreter transaction will use.
--source=SOURCE
    SOURCE is the source the Michelson interpreter transaction will use.
```

### Testing with `test`

The sub-command `test` can be used to test a contract using LIGO. It
differs from `interpret` as in this case we can describe the test
*internally* using LIGO code, and no Michelson code is actually
evaluated.

> ⚠️ Please keep in mind that this sub-command is still BETA, and that
> there are features that are work in progress and are subject to
> change. No real test procedure should rely on this sub-command
> alone.

Resuming the example we used in the explanation for `interpret`, let's
add a new LIGO entry that uses the extra primitives provided by `test`
to test the contract above. This time we will simulate that the
contract is actually deployed to an address, and check inside LIGO
that the resulting storage is `42` after executing a call to
`Increment`:

<Syntax syntax="pascaligo">

```pascaligo skip
const testme = block {
  var addr := Test.originate(main, 10);
  var u := Test.external_call(addr, Increment (32), 0tz)
  } with (Test.get_storage(addr) : int) = 42
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let testme =
  let addr = Test.originate main 10 in
  let u = Test.external_call addr  (Increment (32)) 0tz in
  (Test.get_storage addr : int) = 42
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let testme =
  let addr = Test.originate(main, 10);
  let u = Test.external_call(addr, Increment (32), 0tz);
  (Test.get_storage(addr) : int) == 42;
```

</Syntax>

Notice that now we wrote the property *inside* LIGO, using:

* `Test.originate` to deploy a contract.

* `Test.external_call` to simulate an external call.

* `Test.get_storage` to check the storage from a contract.

A property like `testme` is a definition of a boolean value. The
sub-command `test` evaluates a test, and returns whether it was
successful or not (i.e. returned `true` or `false`).

<Syntax syntax="pascaligo">

```shell
ligo test gitlab-pages/docs/advanced/src/testing/testme.ligo "testme"
// Outputs:
// Test was successful
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo test gitlab-pages/docs/advanced/src/testing/testme.mligo "testme"
// Outputs:
// Test was successful
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo test gitlab-pages/docs/advanced/src/testing/testme.religo "testme"
// Outputs:
// Test was successful
```

</Syntax>

The extra features we can use in LIGO when using the sub-command
`test` are the following:

* `Test.originate c st` binds contract `c` with the address `addr` which is returned, `st` as the initial storage.

* `Test.set_now t` sets the current time to `t`.

* `Test.set_balance addr b` sets the balance of contract bound to address `addr` (returns `unit`).

* `Test.external_call addr p amt` performs a call to contract bound to `addr` with parameter `p` and amount `amt` (returns `unit`).

* `Test.get_storage addr` returns current storage bound to address `addr`.

* `Test.get_balance` returns current balance bound to address `addr`.

* `Test.assert_failure (f : unit -> _)` returns `true` if `f ()` fails.

* `Test.log x` prints `x` into the console.
