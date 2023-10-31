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

## API primer

<SyntaxTitle syntax="cameligo">
val constant : string -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let constant : string => 'a
</SyntaxTitle>

The primitive `Tezos.constant` allows you to use a predefined constant already registered on chain.
It accepts a hash in the form of a string and will require a type annotation.

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

<Syntax syntax="cameligo">

```
(Tezos.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" : int -> int)
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

<Syntax syntax="cameligo">

```shell
ligo compile contract ./gitlab-pages/docs/advanced/src/global_call.mligo --constants "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile contract ./gitlab-pages/docs/advanced/src/global_call.jsligo --constants "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
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

Given the following contract `global_const`:

<Syntax syntax="cameligo">

```cameligo group=global_const
let helper ((s, x) : string * int) =
  String.length s + x * 3 + 2

[@entry]
let main (p : string) (s : int) : operation list * int =
  ([], helper (p, s))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=global_const
const helper = ([s, x]: [string, int]) =>
  String.length(s) + x * 3 + 2;

@entry
const main = (p: string, s: int) : [list<operation>, int] =>
  [list([]), helper ([p, s])];
```

</Syntax>

We want to turn the function `helper` into a global constant. The first
step is to ask LIGO to compile the constant:

<Syntax syntax="cameligo">

```shell
ligo compile constant cameligo "helper" --init-file ./gitlab-pages/docs/advanced/src/global_const.mligo
# Outputs:
# Michelson constant as JSON string:
# "{ UNPAIR ;\n  PUSH int 2 ;\n  PUSH int 3 ;\n  DIG 3 ;\n  MUL ;\n  DIG 2 ;\n  SIZE ;\n  ADD ;\n  ADD }"
# This string can be passed in `--constants` argument when compiling a contract.
# 
# Remember to register it in the network, e.g.:
# > tezos-client register global constant "{ UNPAIR ;
#   PUSH int 2 ;
#   PUSH int 3 ;
#   DIG 3 ;
#   MUL ;
#   DIG 2 ;
#   SIZE ;
#   ADD ;
#   ADD }" from bootstrap1
# 
# Constant hash:
# exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile constant jsligo "helper" --init-file ./gitlab-pages/docs/advanced/src/global_const.jsligo
# Outputs:
# Michelson constant as JSON string:
# "{ UNPAIR ;\n  PUSH int 2 ;\n  PUSH int 3 ;\n  DIG 3 ;\n  MUL ;\n  DIG 2 ;\n  SIZE ;\n  ADD ;\n  ADD }"
# This string can be passed in `--constants` argument when compiling a contract.
# 
# Remember to register it in the network, e.g.:
# > tezos-client register global constant "{ UNPAIR ;
#   PUSH int 2 ;
#   PUSH int 3 ;
#   DIG 3 ;
#   MUL ;
#   DIG 2 ;
#   SIZE ;
#   ADD ;
#   ADD }" from bootstrap1
# 
# Constant hash:
# exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>

As we can see, the constant hash is:

<Syntax syntax="cameligo">

```
exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>

<Syntax syntax="jsligo">

```
exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf
```

</Syntax>

We can now remove the `helper` function from the code, and replace the
references to `helper` by

<Syntax syntax="cameligo">

```
(Tezos.constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" : (string * int) -> int)
```

</Syntax>

<Syntax syntax="jsligo">

```
(Tezos.constant("exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf") as ((_ps : [string, int]) => int))
```

</Syntax>

The new version of `global_call` looks as follows:

<Syntax syntax="cameligo">

```cameligo skip
let main (p : string) (s : int) : operation list * int =
  ([], (Tezos.constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf")(p, s))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const main = (p: string, s: int) : [list<operation>, int] =>
  [ list([]), Tezos.constant("exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf")(p, s) ];
```

</Syntax>

Save the constant's Micheline value in a file `consts.json` (that will be
used when calling `compile contract`). This file must be a JSON list
consisting of the string returned by `compile constant`:

<Syntax syntax="cameligo">

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

<Syntax syntax="cameligo">

```shell
ligo compile contract global_call.mligo --file-constants consts.json
# Outputs:
# { parameter string ;
#   storage int ;
#   code { constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" ;
#          NIL operation ;
#          PAIR } }
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile contract global_call.jsligo --file-constants consts.json
# Outputs:
# { parameter string ;
#   storage int ;
#   code { constant "exprv547Y7U5wKLbQGmkDU9Coh5tKPzvEJjyUed7px9yGt9nrkELXf" ;
#          NIL operation ;
#          PAIR } }
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

<Syntax syntax="cameligo">

```cameligo test-ligo group=test_global
module C = struct
  type storage = int
  type parameter = unit
  type return = operation list * storage

  let f (x : int) = x * 3 + 2

  let ct = Test.register_constant (Test.eval f)

  [@entry]
  let main (() : parameter) (store : storage) : return =
    [], (Tezos.constant ct store)
end

let test =
  let orig = Test.originate (contract_of C) 1 0tez in
  let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
  assert (Test.get_storage orig.addr = 5)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=test_global
namespace C {
  type storage = int
  type parameter = unit

  const f = (x : int) => x * 3 + 2;

  const ct = Test.register_constant(Test.eval(f));

  @entry
  const main = (p: parameter, s: storage) : [list<operation>, storage] =>
    [list([]), Tezos.constant(ct)(s)];
}

const _test = () => {
  let orig = Test.originate(contract_of(C), 1, 0tez);
  Test.transfer_exn(orig.addr, Main(unit), 0tez);
  assert (Test.get_storage(orig.addr) == 5);
};

const test = _test();
```

</Syntax>

<!-- updated use of entry -->