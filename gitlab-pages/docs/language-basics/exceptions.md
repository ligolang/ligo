---
id: exceptions
title: Exceptions
---

import Syntax from '@theme/Syntax';

In some cases it is necessary to interrupt the flow of execution with
a failure: this is where the predefined function `failwith` comes in.

## The `failwith` function

The failwith function raises an error that cannot be caught, which
terminates the contract.

<Syntax syntax="cameligo">

```cameligo group=failwith
type storage = unit

let main (p, store : unit * storage) : operation list * storage =
  (failwith "This contract always fails." : operation list * storage)
```

The call to failwith should be annotated with a type as the type-checker cannot infer the correct type yet.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=failwith
let main = (p: unit, s: unit) : [list<operation>, unit] =>
  failwith("This contract always fails");
```

The call to failwith should be annotated with a type as the type-checker cannot infer the correct type yet.

</Syntax>

## Assertions

Assertions can be used to ensure a certain condition is met when
running a contract. The predefined function `assert` is used to check
whether a given a Boolean condition is true. The function
`assert_some` is used to check if an option value is not `None`. The
function `assert_some_with_error` is like `assert_some` but an error
message can be given. When a condition is not met, the contract will
stop executing and display an error.

<Syntax syntax="cameligo">

```cameligo group=failwith
let main (p, s : bool * unit) : operation list * unit =
  let u : unit = assert p
  in [], s

let some (o : unit option) =
  assert_some o
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=failwith_alt
let main = (p: bool, s: unit) : [list<operation>, unit] => {
  let u: unit = assert(p);
  return [list([]), s];
};

let some = (o: option<unit>) => {
  assert_some(o)
};
```

</Syntax>

You can use `assert_with_error` or `assert_some_with_error` to use a custom error message

<Syntax syntax="cameligo">

```cameligo group=failwith
let main (p, s : bool * unit) : operation list * unit =
  let () = assert_with_error p "My custom error message."
  in [], s
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=failwith
let main2 = (p: bool, s: unit) : [list<operation>, unit] => {
  assert_with_error (p, "My custom error message.");
  return [list([]), s];
};
```

</Syntax>
