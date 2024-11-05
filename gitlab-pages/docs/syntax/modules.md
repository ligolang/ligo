---
id: modules
title: Modules
jsligoTitle: Namespaces
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Modules are collections of related definitions that make them modular and help them avoid name collisions.
A common example of a module is a data type and associated operations over it, such as stacks or queues.
A module can contain the declaration of a type that represents a currency with functions that manipulate the currency and constants that apply to it.
Other pieces of code can access these definitions, providing maintainability, reusability and safety.

:::note

Developers often put a single smart contract in a module, but LIGO does not require you to put a contract in a module and it does not limit modules to contain a single contract.

:::

Modules have some similarities with records because they can both contain multiple definitions.
However, there are significant differences between records and modules:

- Records are expressions and therefore can be used as values, and modules are not expressions and can't be used as values.
For example, you can pass a record as an argument to a function, but you cannot pass a module in this way except in specific circumstances, such using the `contract_of` function to create a contract from a module to use in [Testing](../testing).

- Records cannot package type and value definitions together like modules can.

Which construct you use depends on your design and strategy: modules behave like libraries and records behave like individual units of computation.

## Creating modules

To create a module, declare it with the keyword `module` and a name that starts with a capital letter.
Then put the type, function, value, and nested module definitions in a block between the keyword `struct` and the keyword `end`.

For example, the following code defines a module named `Euro` to represent the Euro currency.
It includes a type (internally called `t`), an operation `add` that sums two values of the given currency, and constants for one and two Euros.

```cameligo group=euro
(* This is euro.mligo *)

module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end
```

To access the contents of a module, use the name of the module and the selection operator "`.`", as with records.
For example, this piece of code in the same file defines a value of the `Euro` type and uses the functions and constants in the module to manipulate it:

```cameligo group=euro
type euro_balance = Euro.t

let add_tip (s : euro_balance) : euro_balance =
  Euro.add (s, Euro.one)
```

In principle, you could change the internal implementation of the `Euro` module without having to change the `euro_balance` type or the `add_tip` function.
For example, if you decide to support manipulating negative values, you could change the `Euro.t` type to an integer:

```cameligo group=euro_sub
module Euro =
  struct
    type t = int
    let add (a, b : t * t) : t = a + b
    let sub (a, b : t * t) : t = a - b
    let one : t = 1
    let two : t = 2
  end
```

The `add_tip` function still works, and no change is needed.
Abstraction accomplished!

However, clients that use the `Euro` module might still break the abstraction if they directly use the underlying representation of `Euro.t`.
For example, the type `Euro.t` is a transparent alias of `nat` (or `int`).
In this case, other code might break if it performs operations that are valid on nats but not on integers.
Client code should always try to respect the interface provided by the module and not make assumptions on its current underlying representation.

By default, all the definitions in a module are "exported", that is, they are accessible from outside the module.
In order to hide the representation of a type in a module, you can constrain the module with a module type, or [*signature*](../signatures/declaring).

</Syntax>

<Syntax syntax="jsligo">

Namespaces are collections of related definitions that make them modular and help them avoid name collisions.
A common example of a namespace is a data type and associated operations over it, such as stacks or queues.
A namespace can contain the declaration of a type that represents a currency with functions that manipulate the currency and constants that apply to it.
Other pieces of code can access these definitions, providing maintainability, reusability and safety.

:::note

Developers often put a single smart contract in a namespace, but LIGO does not require you to put a contract in a namespace and it does not limit namespaces to contain a single contract.

:::

Namespaces have some similarities with records because they can both contain multiple definitions.
However, there are significant differences between records and namespaces:

- Records are expressions and therefore can be used as values, and namespaces are not expressions and can't be used as values.
For example, you can pass a record as an argument to a function, but you cannot pass a namespace in this way except in specific circumstances, such using the `contract_of` function to create a contract from a namespace to use in [Testing](../testing).

- Records cannot package type and value definitions together like namespaces can.

Which construct you use depends on your design and strategy: namespaces behave like libraries and records behave like individual units of computation.

## Creating namespaces

To create a namespace, declare it with the keyword `namespace` and a name that starts with a capital letter.
Then put the type, function, value, and nested namespace definitions in a block opened by "`{`" and closed by "`}`"

For example, the following code defines a namespace named `Euro` to represent the Euro currency.
It packages together a type (internally called `t`), an operation `add` that sums two values of the given currency, and constants for one and two Euros.

```jsligo group=euro
// This is euro.jsligo

namespace Euro {
  export type t = nat;
  export const add = (a: t, b: t) : t => a + b;
  export const one: t = 1n;
  export const two: t = 2n;
};
```

To access the contents of a namespace, use the name of the namespace and the selection operator "`.`", as with records.
For example, this piece of code in the same file defines a value of the `Euro` type and uses the functions and constants in the namespace to manipulate it:

```jsligo group=euro
type euro_balance = Euro.t;

const add_tip = (s: euro_balance): euro_balance =>
  Euro.add(s, Euro.one);
```

In principle, you could change the internal implementation of the `Euro` namespace without having to change the `euro_balance` type or the `add_tip` function.
For example, if you decide to support manipulating negative values, you could change the `Euro.t` type to an integer:

```jsligo group=euro_sub
namespace Euro {
  export type t = int;
  export const add = (a: t, b: t) : t => a + b;
  export const sub = (a: t, b: t) : t => a - b;
  export const one: t = 1;
  export const two: t = 2;
};
```

The `add_tip` function still works, and no change is needed.
Abstraction accomplished!

However, clients that use the `Euro` namespace might still break the abstraction if they directly use the underlying representation of `Euro.t`.
For example, the type `Euro.t` is a transparent alias of `nat` (or `int`).
In this case, other code might break if it performs operations that are valid on nats but not on integers.
Client code should always try to respect the interface provided by the namespace and not make assumptions on its current underlying representation.

</Syntax>

<Syntax syntax="cameligo">

## Nesting modules

You can define a module inside another module.
For example, this version of the `Euro` module groups constants in a sub-module:

```cameligo group=module_nesting
module Euro =
  struct
    type t = nat

    let add (a, b : t * t) : t = a + b

    module Coin =
      struct
        let one : t = 1n
        let two : t = 2n
      end
  end
```

To access nested modules, use the selection operator as many times as necessary:

```cameligo group=module_nesting
type euro_balance = Euro.t

let increment (s : euro_balance) : euro_balance =
  Euro.add (s, Euro.Coin.one)
```

</Syntax>

<Syntax syntax="jsligo">

## Nesting namespaces

You can define a namespace inside another namespace.
For example, this version of the `Euro` namespace groups constants in a sub-namespace:

```jsligo group=namespace_nesting
namespace Euro {
  export type t = nat;

  export let add = (a: t, b: t): t => a + b;

  export namespace Coin {
    export let one: t = 1n;
    export let two: t = 2n;
  };
};
```

To access nested namespaces, use the selection operator as many times as necessary:

```jsligo group=namespace_nesting
type euro_balance = Euro.t;

const increment = (s: euro_balance): euro_balance =>
  Euro.add(s, Euro.Coin.one);
```

Note that the sub-namespace `Coin` must be prefixed by the keyword `export` to make its contents available outside the namespace.

</Syntax>

<Syntax syntax="cameligo">

## Aliasing modules

You can apply an alias to a module to create modules that work as synonyms of previously defined modules.
Creating a synonym of a module can be useful to implement a module that is currently the same as another module for now but may need to change in the future.
For example, until 2025, the Bulgarian Lev is pegged to the euro currency, so the `Bulgarian_Lev` module is an alias of the `Euro` module:

```cameligo group=Lev
module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end

module Bulgarian_Lev = Euro
```

Now other code can use the Lev just like it is a Euro for now, and you can change how the Lev works later.

</Syntax>

<Syntax syntax="jsligo">

## Aliasing namespaces

You can apply an alias to a namespace to create namespaces that work as synonyms of previously defined namespaces.
Creating a synonym of a namespace can be useful to implement a namespace that is currently the same as another namespace for now but may need to change in the future.
For example, until 2025, the Bulgarian Lev is pegged to the euro currency, so the `Bulgarian_Lev` namespace is an alias of the `Euro` namespace:

```jsligo group=Lev
namespace Euro {
  export type t = nat;
  export const add = (a: t, b: t) : t => a + b;
  export const one: t = 1n;
  export const two: t = 2n;
};

import Bulgarian_Lev = Euro;
```

Now other code can use the Lev just like it is a Euro for now, and you can change how the Lev works later.

:::note

You must use the `import` keyword to alias the namespace, even if it is in the same file.

:::

</Syntax>

<Syntax syntax="cameligo">

## Including modules

You can include the content of one module inside another with the `include` keyword.
Including another module in this way can be another way to nest and organize code.
It can also allow you to upgrade or extend a module, such as by adding new types, functions, and values.

This example extends the `Euro` module by including it in a new `NewEuro` module that has a constant for a 10 Euro note:

```cameligo group=including
module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end

module NewEuro =
  struct
    include Euro
    let ten : t = 10n
  end
```

Including modules is not possible in JsLIGO.

</Syntax>

<Syntax syntax="cameligo">

## Importing modules

You can import modules from other files with the `#import` directive.
See [`#import`](../preprocessor/import).

</Syntax>

<Syntax syntax="jsligo">

You can import namespaces from other files with the `#import` directive, but only if the namespaces have the `@public` decorator.
See [`#import`](../preprocessor/import).

</Syntax>
