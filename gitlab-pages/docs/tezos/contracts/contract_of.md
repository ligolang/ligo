---
id: contract_of
title: Contract of module/namespace
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

For technical reasons, contracts are not modules, and modules are not
contracts. In order to use a module as contract, it needs to hold
functions with the type of an entrypoint, these need to be attributed
as `[@entry]`.

When declaring the entry points of a contract using `@entry`, LIGO generates two hidden values in the module:

* an implicit `main` function, which can be obtained using the keyword `contract_of(C)` where `C` is the namespace or module containing the entry points, and
* the input type for that `main` function, which can be obtained using the keyword `parameter_of C`.

In the example below, `contract_of(C)` is returns the implicitly-declared `main` function that calls the `increment` or `decrement` entry points depending on the argument given, and `parameter_of C` is the [variant](https://ligolang.org/docs/language-basics/unit-option-pattern-matching#variant-types) `["Increment", int] | ["Decrement", int]`.





for example for testing, we need the
built-in function `contract_of`. It is built-in (and a keyword)
because it takes a module as parameter, and the type system of LIGO
has also a predefined type for its return value: `('param, 'storage)
module_contract`, but not the full type.

```cameligo group=contract_of
type storage = int
type return = operation list * storage

module C = struct
  [@entry]
  let decrement (param : int) (storage : storage) : return =
    [], storage   - param

  [@entry]
  let increment (param : int) (storage : storage) : return =
    [], storage + param

  [@entry]
  let reset () (_ : storage) : return = [], 0
end

let test_initial_storage () : unit =
  let init_storage = 42 in
  let fee = 0mutez in
  let orig = Test.Next.originate (contract_of C) init_storage fee in
  let new_storage = Test.Next.Typed_address.get_storage orig.taddr
  in assert (new_storage = init_storage)
```

</Syntax>

<Syntax syntax="jsligo">

For technical reasons, contracts are not namespaces, and namespaces
are not contracts. In order to use a namespace that contains functions
with the type of an entrypoint, and that are decorated as `@entry`, as
a contract, for example for testing, we need the built-in function
`contract_of`. It is built-in (and a keyword) because it takes a
module as parameter, and the type system of LIGO has also a predefined
type for its return value: `module_contract<param, storage>`, but not
the full type.

```jsligo group=contract_of
type storage = int;
type @return = [list<operation>, storage];

namespace C {
  @entry
  const decrement = (param: int, storage: storage) : @return =>
    [list([]), storage - param];

  @entry
  const increment = (param: int, storage: storage) : @return =>
    [list([]), storage + param];

  @entry
  const reset = (_unit: unit, _storage: storage) : @return =>
    [list([]), 0];
}

const test_initial_storage = () : unit => {
  const init_storage = 42;
  const fee = 0mutez;
  const orig = Test.Next.originate (contract_of(C), init_storage, fee);
  const new_storage = Test.Next.Typed_address.get_storage(orig.taddr);
  assert(new_storage == init_storage);
}
```

</Syntax>
