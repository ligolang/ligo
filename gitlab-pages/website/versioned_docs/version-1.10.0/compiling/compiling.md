---
id: compiling
title: Compiling contracts
---

import Syntax from '@theme/Syntax';

You must compile LIGO contracts to Michelson before deploying them.
LIGO can also help you compile the initial storage value for a contract and parameters for calls to contracts.

## Compiling contracts

To compile a LIGO contract, make sure that LIGO is installed as described in [Installation](../intro/installation).
Then, pass the source file to the `ligo compile contract` command.

<Syntax syntax="cameligo">

For example, this command compiles a contract that is at the root level of a source file named `my_contract.mligo` and writes it to the file `my_contract.tz`:

```bash
ligo compile contract my_contract.mligo -o my_contract.tz
```

</Syntax>

<Syntax syntax="jsligo">

For example, this command compiles a contract that is at the root level of a source file named `my_contract.jsligo` and writes it to the file `my_contract.tz`:

```bash
ligo compile contract my_contract.jsligo -o my_contract.tz
```

</Syntax>

If the contract is in a module, pass the name of the module to the `-m` argument, as in this example:

<Syntax syntax="cameligo">

```bash
ligo compile contract my_contract.mligo -m MyModule -o my_contract.tz
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo compile contract my_contract.jsligo -m MyModule  -o my_contract.tz
```

</Syntax>

Now you can deploy (originate) the contract; see [Deploying contracts](./deploying).

For more information about the `ligo compile contract` command, see [`compile contract`](../manpages/compile%20contract).

:::info

The `ligo compile contract` command does not automatically run tests in the source file.
To run tests, use the `ligo run test` command as described in [Testing](../testing).

:::

## Compiling storage

When you originate a contract, you set the initial value of the contract storage as a Michelson value.
The `ligo compile storage` command compiles LIGO expressions to Michelson expressions that you can use for the initial value of the storage.

For example, this contract has a complex storage type that includes two records and an address:

<Syntax syntax="cameligo">

```cameligo group=compile_storage
module ComplexStorage = struct
  type my_record = {
    x: int;
    y: int;
    z: int;
  }
  type my_labels = {
    a: string;
    b: string;
    c: string;
  }
  type storage = (my_record * my_labels * address)
  type return_type = operation list * storage

  [@entry]
  let noop (_u : unit) (storage : storage) : return_type = [], storage

end
```

To compile an initial storage value for this contract, create a CameLIGO expression of the storage type and pass it to the `ligo compile storage` command, as in this example:

```bash
ligo compile storage -m ComplexStorage complexStorage.mligo \
  '{x = 1; y = 2; z = 3}, {a = "A"; b = "B"; c = "C"}, ("tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx" : address)'
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=compile_storage
namespace ComplexStorage {
  type my_record = {
    x: int,
    y: int,
    z: int,
  };
  type my_labels = {
    a: string,
    b: string,
    c: string,
  };
  type storage = [my_record, my_labels, address];
  type return_type = [list<operation>, storage];

  @entry
  const noop = (_u: unit, storage: storage): return_type => [[], storage];

}
```

To compile an initial storage value for this contract, create a JsLIGO expression of the storage type and pass it to the `ligo compile storage` command, as in this example:

```bash
ligo compile storage -m ComplexStorage complexStorage.jsligo \
  '[{x: 1, y: 2, z: 3}, {a: "A", b: "B", c: "C"}, "tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx" as address]'
```

</Syntax>

The result is a Michelson expression that you can use as the initial storage value:

```michelson
(Pair (Pair 1 2 3) (Pair "A" "B" "C") "tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx")
```

If you deploy a contract with the `octez-client originate contract` command, you can pass this expression as the value of the `--init` argument, as in this example:

```bash
octez-client originate contract ComplexStorage \
  transferring 0 from my_wallet running ComplexStorage.tz \
  --init '(Pair (Pair 1 2 3) (Pair "A" "B" "C") "tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx")' --burn-cap 2
```

For more information about the `ligo compile storage` command, see [`compile storage`](../manpages/compile%20storage).

## Compiling parameters

If you have the source code of a LIGO contract, you can use the `ligo compile parameter` command to compile the parameter that another contract or client can use to call it.

For example, this contract has an entrypoint that accepts a complex parameter:

<Syntax syntax="cameligo">

```cameligo group=compile_param
module ComplexParam = struct
  type storage = int
  type return_type = operation list * storage
  type paramType = (int list * int list * (string * string))

  let sum_fold = fun (result, i: int * int) -> result + i
  let mult_fold = fun (result, i: int * int) -> result * i

  [@entry]
  let complexmath (param : paramType) (_s : storage) : return_type =
    let list1, list2, str_tuple = param in
    let sum : int = List.fold sum_fold list1 0 in
    let product : int = List.fold mult_fold list2 1 in
    let str1, str2 = str_tuple in
    let string_diff : int = String.length(str2) - String.length(str1) in
    let newVal = (sum + product) * string_diff in
    [], newVal

end
```

To compile a parameter to call this contract, create a CameLIGO expression for the parameter and pass it and the entrypoint name to the `ligo compile parameter` command, as in this example:

```bash
ligo compile parameter -m ComplexParam ComplexParam.mligo 'Complexmath([1; 2; 3], [2; 2; 1], ("three", "fifteen"))'
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=compile_param
namespace ComplexParam {
  type storage = int;
  type returnType = [list<operation>, storage];
  type paramType = [list<int>, list<int>, [string, string]];

  const sum_fold = ([result, i]: [int, int]): int => result + i;
  const mult_fold = ([result, i]: [int, int]): int => result * i;

  @entry
  const complexmath = (param: paramType, _s: storage): returnType => {
    const [list1, list2, str_tuple] = param;
    const sum: int = List.fold(sum_fold, list1, 0);
    const product: int = List.fold(mult_fold, list2, 1);
    const [str1, str2] = str_tuple;
    const string_diff: int = String.length(str2) - String.length(str1);
    const newVal = (sum + product) * string_diff;
    return [list([]), newVal];
  }

}
```

To compile a parameter to call this contract, create a JsLIGO expression for the parameter and pass it and the entrypoint name to the `ligo compile parameter` command, as in this example:

```bash
ligo compile parameter ComplexParam.jsligo -m ComplexParam 'Complexmath([[1, 2, 3], [2, 2, 1], ["three", "fifteen"]])'
```

</Syntax>

The result is a Michelson expression that represents passing the value to the entrypoint:

```michelson
(Pair { 1 ; 2 ; 3 } { 2 ; 2 ; 1 } "three" "fifteen")
```

You can use this expression as the parameter to call the contract, as in this example:

If you deploy a contract with the `octez-client originate contract` command, you can pass this expression as the value of the `--init` argument, as in this example:

```bash
octez-client transfer 0 from my_wallet to ComplexParam \
  --arg '(Pair { 1 ; 2 ; 3 } { 2 ; 2 ; 1 } "three" "fifteen")' \
  --burn-cap 1
```

For more information about the `ligo compile parameter` command, see [`compile parameter`](../manpages/compile%20parameter).
