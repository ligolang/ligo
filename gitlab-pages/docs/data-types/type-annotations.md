---
id: type-annotations
title: Type annotations
---

import Syntax from '@theme/Syntax';

As described in [Types](./types), the LIGO compiler must know the type of each variable to be able to compile to Michelson, which is strongly and statically typed.
LIGO can infer or assume the types of variables in many cases, but in cases where it can't, you can manually annotate the types of variables, as in TypeScript and Ocaml.
Annotating types can also improve code readability.

## Annotating types

<Syntax syntax="cameligo">

As in Ocaml, you can annotate a variable with its type by adding a colon and the type or type alias:

```cameligo group=annotate
let myInteger : int = 5
let myString : string = "Hello"
```

More complex types such as lists and maps take sub-types, as in these examples:

```cameligo group=annotate
let myList : int list = [1; 2; 3]
let myMap : (string, int) map =
  Map.literal [
    ("one", 1);
    ("two", 2);
  ]
```

For readability, contracts often annotate function and entrypoint parameters and return types, as in this example:

```cameligo group=annotate
module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let add (value : int) (storage: storage_type) : return_type =
    [], storage + value

  [@entry]
  let sub (value : int) (storage: storage_type) : return_type =
    [], storage - value

end
```

</Syntax>

<Syntax syntax="jsligo">

As in TypeScript, you can annotate a variable with its type by adding a colon and the type or type alias:

```jsligo group=annotate
const myInteger: int = 5;
const myString: string = "Hello";
```

Similarly, you can annotate the value that you assign to a variable:

```jsligo group=annotate
const myOtherInteger = 5 as int;
const myOtherString = "Hello" as string;
```

More complex types such as lists and maps take sub-types in angle brackets, as in these examples:

```jsligo group=annotate
const myList: list<int> = [1, 2, 3];
const myMap: map<string, int> =
  Map.literal([
    ["one", 1],
    ["two", 2],
  ]);
```

For readability, contracts often annotate function and entrypoint parameters and return types, as in this example:

```jsligo group=annotate
namespace Counter {
  type storage_type = int;
  type return_type = [list<operation>, storage_type];

  // @entry
  const add = (value: int, storage: storage_type): return_type =>
    [[], storage + value];

  // @entry
  const sub = (value: int, storage: storage_type): return_type =>
    [[], storage - value];

}
```

</Syntax>

## Inferring types

Sometimes, the LIGO compiler can infer a variable's type from the context.

The following example subtracts two nats and puts the result in a variable.
This variable does not have an explicit type declaration, but the compiler infers that it is a number and that it is an int because the value could be negative, even though in this case it is positive:

<Syntax syntax="cameligo">

```cameligo group=inferring
let a : nat = 7
let b : nat = 5
let c = a - b (* Inferred to be an int *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=inferring
const a: nat = 7;
const b: nat = 5;
const c = a - b; // Inferred to be an int
```

</Syntax>

## Type assumptions

In some cases, the LIGO compiler assumes a variable's type when it does not have complete information.

For example, when LIGO knows that a variable is a number but not whether that number is an integer or a nat, it assumes that it is an integer.
Similarly, LIGO assumes that a list of literal values in brackets is a tuple, not a list, unless you cast that list with the `list` function.

The following contract has two entrypoints that each accept one parameter.
The types of the parameters are not specified, but the compiler can infer that they are numbers by how they are used.
From there, it assumes that they are integers and therefore types them as integers in the compiled contract.

<Syntax syntax="cameligo">

```cameligo group=assumptions
module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  (* The type of the value parameter is assumed to be an int *)
  let add (value) (storage: storage_type) : return_type =
    [], storage + value

  [@entry]
  (* The type of the value parameter is assumed to be an int *)
  let sub (value) (storage: storage_type) : return_type =
    [], storage - value

end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=assumptions
namespace Counter {
  type storage_type = int;
  type return_type = [list<operation>, storage_type];

  // @entry
  // The type of the value parameter is assumed to be an int
  const add = (value, storage: storage_type): return_type =>
    [[], storage + value];

  // @entry
  // The type of the value parameter is assumed to be an int
  const sub = (value, storage: storage_type): return_type =>
    [[], storage - value];

}
```

</Syntax>


<!-- updated use of entry -->
