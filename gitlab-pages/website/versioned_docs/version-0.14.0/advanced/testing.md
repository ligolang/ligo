---
id: testing
title: Testing LIGO
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

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
<Syntax syntax="jsligo">

```jsligo
// This is testme.jsligo
type storage = int;

type parameter =
  ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list<operation>, storage];

// Two entrypoints
let add = ([store, delta]: [storage, int]): storage => store + delta;
let sub = ([store, delta]: [storage, int]): storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ([action, store]: [parameter, storage]) : return_ => {
  return [
    list([]) as list<operation>,    // No operations
    match(action, {
      Increment:(n: int) => add ([store, n]),
      Decrement:(n: int) => sub ([store, n]),
      Reset: ()          => 0})
  ]
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
ligo interpret --init-file testme.mligo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo interpret --init-file testme.religo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo interpret --init-file testme.jsligo "main (Increment (32), 10)"
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

To test the contract we need to create a testing file. This file has
access to an additional `Test` module. The test file is interpreted, 
and implicitly updates a global state (the tezos context). To do that, 
the LIGO interpreter uses the [same library that Tezos internally uses for 
testing](https://gitlab.com/tezos/tezos/-/tree/master/src/proto_alpha/lib_protocol/test/helpers). 
Here we will simulate that the contract is actually deployed to an address, and 
check that the resulting storage is `42` after executing a call to `Increment`:

> Note: the types present in the context of the testing file differ from the 
> ones when writing a contract. 

<Syntax syntax="pascaligo">

```pascaligo skip
const testme_test = "./gitlab-pages/docs/advanced/src/testme.ligo"

const test = block {
  const init_storage = Test.compile_expression (Some(testme_test), [%pascaligo ({| (10 : int) |} : ligo_program) ]);
  const originated_contract = Test.originate(testme_test, "main", init_storage);
  const addr = originated_contract.0;
  const param = Test.compile_expression (Some (testme_test), [%pascaligo ({| Increment(32) |} : ligo_program)]);
  const transfer_result = Test.transfer(addr, param, 0n);
  const result = Test.get_storage(addr);
  const check = Test.compile_expression ((None : option(string)), [%pascaligo ({| (42: int) |} : ligo_program)]);
  Test.log(result);
} with (Test.michelson_equal(result, check))


```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let testme_test = "./gitlab-pages/docs/advanced/src/testme.mligo"

let test =
  let init_storage = Test.compile_expression (Some testme_test) [%cameligo ({| (10 : int) |} : ligo_program) ] in
  let (addr, _, _) = Test.originate testme_test "main" init_storage in
  let param = Test.compile_expression (Some testme_test) [%cameligo ({| Increment(32) |} : ligo_program)] in
  let transfer_result = Test.transfer addr param 0n in
  let result = Test.get_storage addr in
  let check_ = Test.compile_expression (None : string option) [%cameligo ({| (42: int) |} : ligo_program)] in
  let _ = Test.log result in
  Test.michelson_equal result check_
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let testme_test = "./gitlab-pages/docs/advanced/src/testme.religo"

let test = {
  let init_storage = Test.compile_expression(Some(testme_test), [%reasonligo ({| (10 : int) |} : ligo_program) ]);
  let (addr, _, _) = Test.originate(testme_test, "main", init_storage);
  let param = Test.compile_expression((Some testme_test), [%reasonligo ({| Increment(32) |} : ligo_program)]);
  let transfer_result = Test.transfer(addr, param, 0n);
  let result = Test.get_storage(addr);
  let check_ = Test.compile_expression((None : option(string)), [%reasonligo ({| (42: int) |} : ligo_program)]);
  let _ = Test.log(result);
  Test.michelson_equal(result, check_)
}
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let testme_test = "./gitlab-pages/docs/advanced/src/testme.jsligo"

let test_code = (): bool => {
  let init_storage = Test.compile_expression(Some(testme_test), jsligo`10 as int` as ligo_program);
  let [addr, _, _] = Test.originate(testme_test, "main", init_storage);
  let param = Test.compile_expression(Some(testme_test), jsligo`Increment(32)` as ligo_program);
  let transfer_result = Test.transfer(addr, param, 0 as nat);
  let result = Test.get_storage(addr);
  let check_ = Test.compile_expression((None() as option<string>), jsligo`42 as int` as ligo_program);
  Test.log("okay");
  return Test.michelson_equal(result, check_)
}

let test = test_code()
```

</Syntax>

Notice that now we wrote the test property *inside* LIGO, using:

* `Test.compile_expression` to compile an expression.

* `Test.originate` to deploy a contract.

* `Test.transfer` to simulate an external call.

* `Test.get_storage` to check the storage from a contract.

* `Test.log` to log variables.

* `Test.michelson_equal` to check if the Michelson results are equal.




A property like `testme` is a definition of a boolean value. The
sub-command `test` evaluates a test, and returns whether it was
successful or not (i.e. returned `true` or `false`).

<Syntax syntax="pascaligo">

```shell
ligo test gitlab-pages/docs/advanced/src/test.ligo "test"
// Outputs:
// Test passed with true
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo test gitlab-pages/docs/advanced/src/test.mligo "test"
// Outputs:
// Test passed with true
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo test gitlab-pages/docs/advanced/src/test.religo "test"
// Outputs:
// Test passed with true
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo test gitlab-pages/docs/advanced/src/test.jsligo "test"
// Outputs:
// Test passed with true
```

</Syntax>

[More info about the `Test` module available when using the sub-command `test`.](../reference/test.md)
