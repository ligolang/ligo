---
id: global-constants
title: Global constants
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

Since the update to Hangzhou protocol, global constants can be
registered on chain. These global constants are Micheline values
stored on chain, and they can be referred to from a contract we are
deploying. Using global constants, we will be able to originate
contracts that (after expansion) surpass size limit for contracts.

## Using global constants

Global constants are introduced using `Tezos.constant`. This function
expects a _constant hash_, i.e. a hash that corresponds to a
particular constant that was registered on network.

For instance, the constant hash
`expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2` corresponds
to the Michelson code

```
{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }
```

In a contract, we can make reference to a global constant by using the
`Tezos.constant` operation:

<Syntax syntax="pascaligo">

```
(Tezos.constant("expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2") : int -> int)
```

</Syntax>
<Syntax syntax="cameligo">

```
(Tezos.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" : int -> int)
```

</Syntax>
<Syntax syntax="reasonligo">

```
(Tezos.constant(("expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2")) : int => int))
```

</Syntax>
<Syntax syntax="jsligo">

```
(Tezos.constant("expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2") as ((_p : int) => int))
```

</Syntax>

Note that the constant's type needs to be annotated.

When we compile a contract, we need to tell LIGO (and Michelson
type-checker) which are the constants that we are assuming to be
already registered in the context. They are passed to the `compile
contract` sub-command in the `--constants` argument:

<Syntax syntax="pascaligo">

```shell
ligo compile contract global_call.ligo --constants "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile contract global_call.mligo --constants "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile contract global_call.religo --constants "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo compile contract global_call.jsligo --constants "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
```

</Syntax>

## Registering global constants with `tezos-client`

Global constants can be registered on the chain by using `tezos-client` as follows:

```
tezos-client register global constant "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" from bootstrap1
```

which will register the global constant with source `bootstrap1`.

## Compiling global constants

In general, we can compile a constant doing the following steps:

- Use `compile expression` sub-command to compile the expression we are interested in capturing in a constant.

- Register the output of `compile expression` using `tezos-client`.

- Capture the constant hash given by `tezos-client` and use it in the code with `Tezos.constant`.

- Compile the contract that uses the hash constant by passing the argument `--constants`.

In case that `tezos-client` is not available, LIGO provides a custom
sub-command to compile constants, which works similar to the `compile
expression` sub-command, but has the following differences:

- The output is given as a escaped JSON string that can be given directly to the `--constants` argument of `compile contract` (or put in a JSON list in a file passed to `--file-constants`).

- The hash of the constant is also given in output, so that it can be used without need to call `tezos-client`.

:::info
For LIGO users, we recommend to use `compile constant` as it
simplifies the usage flow (see example below).
:::

## Usage example

Given the following contract `global_call`:

<Syntax syntax="pascaligo">

```pascaligo group=pre_global
function helper (const s : string; const x : int) : int is
  String.length(s) + x * 3 + 2

function main(const p : string; const s : int) : list(operation) * int is
  ((nil : list(operation)), helper(p, s))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=pre_global
let helper ((s, x) : string * int) : int =
  String.length s + x * 3 + 2

let main ((p, s) : string * int) : operation list * int =
  (([] : operation list), helper (p, s))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=pre_global
let helper = ((s, x) : (string, int)) : int =>
  String.length(s) + x * 3 + 2;

let main = ((p, s) : (string, int)) : (list (operation), int) =>
  (([] : list (operation)), helper (p, s));
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=pre_global
const helper = ([s, x] : [string, int]) : int =>
  String.length(s) + x * 3 + 2;

const main = ([p, s] : [string, int]) : [list<operation>, int] =>
  [(list([]) as list<operation>), helper (p, s)];
```

</Syntax>

We want to turn the function `helper` into a global constant. The first
step is to ask LIGO to compile the constant:

<Syntax syntax="pascaligo">

```shell
ligo compile constant pascaligo "helper" --init-file global_call.ligo
// Outputs:
// Michelson constant as JSON string:
// "{ UNPAIR ;\n  PUSH int 2 ;\n  PUSH int 3 ;\n  DIG 3 ;\n  MUL ;\n  DIG 2 ;\n  SIZE ;\n  ADD ;\n  ADD }"
// This string can be passed in `--constants` argument when compiling a contract.
//
// Remember to register it in the network, e.g.:
// > tezos-client register global constant "{ UNPAIR ;
//   PUSH int 2 ;
//   PUSH int 3 ;
//   DIG 3 ;
//   MUL ;
//   DIG 2 ;
//   SIZE ;
//   ADD ;
//   ADD }" from bootstrap1
//
// Constant hash:
// exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile constant cameligo "helper" --init-file global_call.mligo
// Outputs:
// Michelson constant as JSON string:
// "{ UNPAIR ;\n  PUSH int 2 ;\n  PUSH int 3 ;\n  DIG 3 ;\n  MUL ;\n  DIG 2 ;\n  SIZE ;\n  ADD ;\n  ADD }"
// This string can be passed in `--constants` argument when compiling a contract.
//
// Remember to register it in the network, e.g.:
// > tezos-client register global constant "{ UNPAIR ;
//   PUSH int 2 ;
//   PUSH int 3 ;
//   DIG 3 ;
//   MUL ;
//   DIG 2 ;
//   SIZE ;
//   ADD ;
//   ADD }" from bootstrap1
//
// Constant hash:
// exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile constant reasonligo "helper" --init-file global_call.religo
// Outputs:
// Michelson constant as JSON string:
// "{ UNPAIR ;\n  PUSH int 2 ;\n  PUSH int 3 ;\n  DIG 3 ;\n  MUL ;\n  DIG 2 ;\n  SIZE ;\n  ADD ;\n  ADD }"
// This string can be passed in `--constants` argument when compiling a contract.
//
// Remember to register it in the network, e.g.:
// > tezos-client register global constant "{ UNPAIR ;
//   PUSH int 2 ;
//   PUSH int 3 ;
//   DIG 3 ;
//   MUL ;
//   DIG 2 ;
//   SIZE ;
//   ADD ;
//   ADD }" from bootstrap1
//
// Constant hash:
// exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo compile constant jsligo "helper" --init-file global_const.jsligo
// Outputs:
// Michelson constant as JSON string:
// "{ PUSH int 2 ;\n  PUSH int 3 ;\n  DUP 3 ;\n  CDR ;\n  MUL ;\n  DIG 2 ;\n  CAR ;\n  SIZE ;\n  ADD ;\n  ADD }"
// This string can be passed in `--constants` argument when compiling a contract.
//
// Remember to register it in the network, e.g.:
// > tezos-client register global constant "{ PUSH int 2 ;
//   PUSH int 3 ;
//   DUP 3 ;
//   CDR ;
//   MUL ;
//   DIG 2 ;
//   CAR ;
//   SIZE ;
//   ADD ;
//   ADD }" from bootstrap1
//
// Constant hash:
// expru4G4gV3ppCneKsDec8s5oTHE1ukSVD6vKb13hBEsqD1xQUvib8
```

</Syntax>

As we can see, the constant hash is:

<Syntax syntax="pascaligo">

```
exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>
<Syntax syntax="cameligo">

```
exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>
<Syntax syntax="reasonligo">

```
exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>
<Syntax syntax="jsligo">

```
expru4G4gV3ppCneKsDec8s5oTHE1ukSVD6vKb13hBEsqD1xQUvib8
```

</Syntax>

We can now remove the `helper` function from the code, and replace the
references to `helper` by

<Syntax syntax="pascaligo">

```
(Tezos.constant("exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf") : string * int -> int)
```

</Syntax>
<Syntax syntax="cameligo">

```
(Tezos.constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" : (string * int) -> int)
```

</Syntax>
<Syntax syntax="reasonligo">

```
(Tezos.constant(("exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf")) : ((string, int) => int))
```

</Syntax>
<Syntax syntax="jsligo">

```
(Tezos.constant("expru4G4gV3ppCneKsDec8s5oTHE1ukSVD6vKb13hBEsqD1xQUvib8") as ((_ps : [string, int]) => int))
```

</Syntax>

The new version of `global_call` looks as follows:

<Syntax syntax="pascaligo">

```pascaligo skip
function main(const p : string; const s : int) : list(operation) * int is
  ((nil : list(operation)), ((Tezos.constant("exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf") : string * int -> int))(p, s))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let main ((p, s) : string * int) : operation list * int =
  (([] : operation list), (Tezos.constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" : (string * int) -> int)(p, s))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let main = ((p, s) : (string, int)) : (list (operation), int) =>
  (([] : list (operation)), (((Tezos.constant(("exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf")) : ((string, int) => int)))(p, s)));
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
const main = ([p, s] : [string, int]) : [list<operation>, int] =>
  [(list([]) as list<operation>), (Tezos.constant("expru4G4gV3ppCneKsDec8s5oTHE1ukSVD6vKb13hBEsqD1xQUvib8") as ((_ps : [string, int]) => int))([p, s])];
```

</Syntax>

Save the constant's Micheline value in a file `consts.json` (that will be
used when calling `compile contract`). This file must be a JSON list
consisting of the string returned by `compile constant`:

<Syntax syntax="pascaligo">

```
["{ UNPAIR ;\n  PUSH int 2 ;\n  PUSH int 3 ;\n  DIG 3 ;\n  MUL ;\n  DIG 2 ;\n  SIZE ;\n  ADD ;\n  ADD }"]
```

</Syntax>
<Syntax syntax="cameligo">

```
["{ UNPAIR ;\n  PUSH int 2 ;\n  PUSH int 3 ;\n  DIG 3 ;\n  MUL ;\n  DIG 2 ;\n  SIZE ;\n  ADD ;\n  ADD }"]
```

</Syntax>
<Syntax syntax="reasonligo">

```
["{ UNPAIR ;\n  PUSH int 2 ;\n  PUSH int 3 ;\n  DIG 3 ;\n  MUL ;\n  DIG 2 ;\n  SIZE ;\n  ADD ;\n  ADD }"]
```

</Syntax>
<Syntax syntax="jsligo">

```
["{ PUSH int 2 ;\n  PUSH int 3 ;\n  DUP 3 ;\n  CDR ;\n  MUL ;\n  DIG 2 ;\n  CAR ;\n  SIZE ;\n  ADD ;\n  ADD }"]
```

</Syntax>

We can compile the code using the `compile contract` sub-command,
passing the file with constants in the flag `--file-constants`:

<Syntax syntax="pascaligo">

```shell
ligo compile contract global_call.ligo --file-constants consts.json
// Outputs:
// { parameter string ;
//   storage int ;
//   code { constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" ;
//          NIL operation ;
//          PAIR } }
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile contract global_call.mligo --file-constants consts.json
// Outputs:
// { parameter string ;
//   storage int ;
//   code { constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" ;
//          NIL operation ;
//          PAIR } }
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile contract global_call.religo --file-constants consts.json
// Outputs:
// { parameter string ;
//   storage int ;
//   code { constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" ;
//          NIL operation ;
//          PAIR } }
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo compile contract global_call.jsligo --file-constants consts.json
// Outputs:
// { parameter string ;
//   storage int ;
//   code { constant "expru4G4gV3ppCneKsDec8s5oTHE1ukSVD6vKb13hBEsqD1xQUvib8" ;
//          NIL operation ;
//          PAIR } }
```

</Syntax>

:::info
Remember that to deploy this contract, first the global constant needs
to be registered in the network where we want to deploy it.
:::

## Global constants in the testing framework

In the testing framework, global constants can be registered to the
context by using `Test.register_constant`. This primitive takes a
`michelson_program` representing the constant to be registered, and
returns an string that is the constant hash corresponding to the
constant registered.

The string returned by `Test.register_constant` can be used via
`Tezos.constant`, as in the examples above.

A simple usage case is the following, in which we obtain a
`michelson_program` by using `Test.eval`:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=test_global
type storage is int
type parameter is unit

function f(const x : int) : int is x * 3 + 2;

const ct : string = Test.register_constant(Test.eval(f));

function main(const p : parameter; const s : storage) is
  ((nil : list(operation)), (((Tezos.constant(ct) : int -> int))(s)));

const test = {
  const (taddr, _, _) = Test.originate(main, 1, 0tez);
  const ctr = Test.to_contract(taddr);
  const _ = Test.transfer_to_contract_exn(ctr, Unit, 0tez);
} with assert(Test.get_storage(taddr) = 5);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=test_global
type storage = int
type parameter = unit
type return = operation list * storage

let f (x : int) = x * 3 + 2

let ct : string = Test.register_constant (Test.eval f)

let main ((), store : parameter * storage) : return =
 ([] : operation list), ((Tezos.constant ct : int -> int) store)

let test =
  let (taddr, _, _) = Test.originate main 1 0tez in
  let ctr = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn ctr () 0tez in
  assert (Test.get_storage taddr = 5)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=test_global
type storage = int
type parameter = unit

let f = (x : int) => x * 3 + 2;

let ct : string = Test.register_constant(Test.eval(f));

let main = ((p, s) : (parameter, storage)) : (list (operation), storage) =>
  (([] : list (operation)), (((Tezos.constant(ct) : (int => int)))(s)));

let test =
  let (taddr, _, _) = Test.originate(main, 1, 0tez);
  let ctr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(ctr, (), 0tez);
  assert (Test.get_storage(taddr) == 5);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=test_global
type storage = int
type parameter = unit

const f = (x : int) : int => x * 3 + 2;

const ct : string = Test.register_constant(Test.eval(f));

const main = ([p, s] : [parameter, storage]) : [list<operation>, storage] =>
  [(list([]) as list<operation>), (((Tezos.constant(ct) as ((x:int) => int)))(s))];

const _test = (_u : unit) : unit => {
  let [taddr, _, _] = Test.originate(main, 1, (0 as tez));
  let ctr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(ctr, unit, (0 as tez));
  assert (Test.get_storage(taddr) == 5);
};

const test = _test();
```

</Syntax>
