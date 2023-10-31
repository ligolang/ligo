---
id: modules
title: Modules
---

import Syntax from '@theme/Syntax';

<Syntax syntax="jsligo">

> Note that in JsLIGO modules are called `namespaces`.

</Syntax>

Modules are a programming language construction that allows us to
package related definitions together. A canonical example of a module
is a data type and associated operations over it (e.g. stacks or
queues). The rest of the program can access these definitions in a
regular and abstract way, providing maintainability, reusability and
safety.

For a concrete example, we could create a module that packages a type
that represents amounts in a particular currency together with
functions that manipulate these amounts: constants, addition,
subtraction, etc. A piece of code that uses this module can be
agnostic concerning how the type is actually represented inside the
module: it is said to be abstract.

## Declaring Modules

<Syntax syntax="cameligo">

Modules are introduced using the `module` keyword. For example, the
following code defines a module `EURO` that packages together a type,
called `t`, together with an operation `add` that sums two values of
the given currency, as well as constants for zero and one.

```cameligo group=EURO
module EURO =
  struct
    type t = nat
    let add (a , b : t * t) : t = a + b
    let zero : t = 0n
    let one : t = 1n
  end
```

As we can see, in CameLIGO we also use a `struct ... end` block to
group together the definitions made in the module.

</Syntax>

<Syntax syntax="jsligo">

Modules are introduced using the `namespace` keyword. For example, the
following code defines a module `EURO` that packages together a type,
called `t`, together with an operation `add` that sums two values of
the given currency, as well as constants for zero and one.

```jsligo group=EURO
namespace EURO {
  export type t = nat;
  export const add = (a: t, b: t) : t => a + b;
  export const zero: t = 0n;
  export const one: t = 1n
}
```

In this example you will also notice the `export` keyword. A statement
within a module can be accessed from outside the module if it is
exported.

</Syntax>

## Using Modules

We can access a module's components by using the selection operator
`.`.  Let us suppose that our storage keeps a value in euros using the
previously defined module `EURO`. Then, we can write a `main` entry
point that increments the storage value each time it is called.

<Syntax syntax="cameligo">

```cameligo group=EURO
type storage = EURO.t

[@entry]
let main (_action : unit) (store : storage) : operation list * storage =
  [], EURO.add (store, EURO.one)
```
</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO
type storage = EURO.t;

@entry
let main = (_action: unit, store: storage): [list<operation>, storage] =>
  [list([]), EURO.add (store, EURO.one)];
```

</Syntax>

In principle, we could change the implementation of `EURO`, without
having to change the `storage` type or the function `main`. For
example, if we decide later that we should support manipulating
negative values, we could change `EURO` as follows:

<Syntax syntax="cameligo">

```cameligo group=EURO2
module EURO =
  struct
    type t = int
    let add (a, b : t * t) : t = a + b
    let zero : t = 0
    let one : t = 1
  end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO2
namespace EURO {
  export type t = int;
  export const add = (a: t, b: t) : t => a + b;
  export const zero: t = 0;
  export const one: t = 1;
}
```

</Syntax>

Notice that the code in `main` still works, and no change is
needed. Abstraction accomplished!

> ⚠️ Please note that code using the module `EURO` might still break
> the abstraction if it directly uses the underlying representation of
> `EURO.t`. Client code should always try to respect the interface
> provided by the module, and not make assumptions on its current
> underlying representation (e.g. `EURO.t` is a transparent alias
> of `nat`; future versons of LIGO might make this an opaque/abstract type).

## Nested Modules: Sub-Modules

Modules can be nested, which means that we can define a module inside
another module. Let's see how that works, and define a variant of
`EURO` in which the constants are all grouped inside using a sub-module.

<Syntax syntax="cameligo">

```cameligo group=EURO3
module EURO =
  struct
    type t = nat

    let add (a, b : t * t) : t = a + b

    module CONST =
      struct
        let zero : t = 0n
        let one : t = 1n
      end
  end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO3
namespace EURO {
  export type t = nat;

  export let add = (a: t, b: t): t => a + b;

  export namespace CONST {
    export let zero: t = 0n;
    export let one: t = 1n;
  };
};
```

</Syntax>

To access nested modules we simply apply the selection operator more
than once:

<Syntax syntax="cameligo">

```cameligo group=EURO3
type storage = EURO.t

[@entry]
let main (_action : unit) (store : storage) : operation list * storage =
 [], EURO.add (store, EURO.CONST.one)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO3
type storage = EURO.t;

@entry
let main = (_action: unit, store: storage) : [list<operation>, storage] =>
 [list([]), EURO.add (store, EURO.CONST.one)]
```

</Syntax>

## Modules and Imports: Build System

Modules also allow us to separate our code in different files: when we
import a file, we obtain a module encapsulating all the definitions in
it. This will become very handy for organising large contracts, as we
can divide it into different files, and the module system keeps the naming
space clean.

Generally, we will take a set of definitions that can be naturally
grouped by functionality, and put them together in a separate
file.

<Syntax syntax="cameligo">

For example, in CameLIGO, we can create a file `imported.mligo`:

```cameligo group=imported
type t = nat

let add (a , b : t * t) : t = a + b

let zero : t = 0n
let one : t = 1n
```

</Syntax>

<Syntax syntax="jsligo">

For example, in JsLIGO, we can create a file `imported.jsligo`:

```jsligo group=imported
export type t = nat;

export const add = (a: t, b: t): t => a + b;

export const zero: t = 0n;
export const one: t = 1n;
```

</Syntax>

<Syntax syntax="cameligo">

Later, in another file, we can import `imported.mligo` as a module, and
use its definitions. For example, we could create a `importer.mligo`
that imports all definitions from `imported.mligo` as the module
`EURO`:

```cameligo
#import "./gitlab-pages/docs/language-basics/src/modules/imported.mligo" "EURO"

type storage = EURO.t

[@entry]
let main (_action : unit) (store : storage) : operation list * storage =
 ([], EURO.add(store, EURO.one))
```

</Syntax>

<Syntax syntax="jsligo">

Later, in another file, we can import `imported.jsligo` as a module, and
use its definitions. For example, we could create a `importer.jsligo`
that imports all definitions from `imported.jsligo` as the module
`EURO`:

```jsligo
#import "./gitlab-pages/docs/language-basics/src/modules/imported.jsligo" "EURO"

type storage = EURO.t;

@entry
const main = (_action: unit, store: storage): [list<operation>, storage] =>
  [list([]), EURO.add(store, EURO.one)];
```

</Syntax>

We can compile the file that uses the `#import` statement directly,
without having to mention the imported file.

<Syntax syntax="cameligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/importer.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/importer.jsligo
```

</Syntax>


## Module Aliases

LIGO supports module aliases, that is, modules that work as synonyms
to other (previously defined) modules. This feature can be useful if
we could implement a module using a previously defined one, but in the
future, we might need to change it.

<Syntax syntax="cameligo">

```cameligo group=EURO
module US_DOLLAR = EURO
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO
import US_DOLLAR = EURO;
```

</Syntax>


## Modules as Contracts

<Syntax syntax="cameligo">

When a module contains declarations that are tagged with the attribute
`@entry`, then a contract can be obtained from such module. All
declarations in the module tagged as `@entry` are grouped, and a
dispatcher contract is generated.

```cameligo group=contract
module C = struct
  [@entry] let increment (p : int) (s : int) : operation list * int = [], s + p
  [@entry] let decrement (p : int) (s : int) : operation list * int = [], s - p
end
```

</Syntax>

<Syntax syntax="jsligo">

When a module contains declarations that are tagged with the `@entry`
decorator, then a contract can be obtained from such module. All
declarations in the module tagged as `@entry` are grouped, and a
dispatcher contract is generated.

```jsligo group=contract
namespace C {
  @entry
  const increment = (p : int, s : int) : [list<operation>, int] => [list([]), s + p];
  @entry
  const decrement = (p : int, s : int) : [list<operation>, int] => [list([]), s - p];
};
```

</Syntax>

A module can be compiled as a contract using `-m`:

<Syntax syntax="cameligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/contract.mligo -m C
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/contract.jsligo -m C
```

</Syntax>

To access the contract from the module, the primitive `contract_of`
can be used. The type of the parameter generated for the module can be
obtaining using the primitive `parameter_of`. This is particularly
useful when working with the testing framework, in conjunction with the
function `Test.originate`:

<Syntax syntax="cameligo">

```cameligo group=contract
let test =
  let orig = Test.originate (contract_of C) 0 0tez in
  let _ = Test.transfer_exn orig.addr (Increment 42) 0tez
  in assert (42 = Test.get_storage orig.addr)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=contract
const test = do {
  let orig = Test.originate(contract_of(C), 0, 0tez);
  Test.transfer_exn(orig.addr, (Increment (42)), 1mutez);
  return assert(Test.get_storage(orig.addr) == 42);
};
```

</Syntax>

## Module Inclusion

<Syntax syntax="cameligo">
When writing a new version of a given contract derived from a module,
it is often needed to add new features, that is, new types and values,
for example when implementing the next version of a standard. This can
be achieved by defining a new module that includes the types and
values of the old one, and defines new ones.

The inclusion of a module `M` is specified with a field `include M`,
like so:

```cameligo group=contract
module FA0 = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end

module FA0Ext = struct
  include FA0
  [@entry] let transfer2 (a : unit) (b : t) = transfer a b
end
```

</Syntax>

<Syntax syntax="jsligo">
This feature is not available in JsLIGO.
</Syntax>

## Module Types

<Syntax syntax="cameligo">

Until now, we dealt with implicit module types, also know as
signatures. Having explicitly declared module types enable abstraction
and reusability by inclusion of signatures. Module types are defined
like in OCaml:

```cameligo group=contract2
module type FA0_SIG = sig
  type t
  [@entry] val transfer : unit -> t -> operation list * t
end

module type FA0Ext_SIG = sig
  include FA0_SIG
  [@entry] val transfer2 : unit -> t -> operation list * t
end
```

Notice how `t` in the type of `transfer2` refers to `t` in the
signature `FA0_SIG` and remains abstract. We can now revisit the
examples above by constraining the module definitions with the module
types:

```cameligo group=contract2
module FA0 : FA0_SIG = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end

module FA0Ext : FA0Ext_SIG = struct
  include FA0
  [@entry] let transfer2 (a : unit) (b : t) = transfer a b
end
```

Note how module definitions must instantiate any abstract type (here
`FA0Impl.t`). Also, when a module is constrained by a signature, it
must implement the types and values in the latter, but no more: this
is a filtering semantics.
</Syntax>

<Syntax syntax="jsligo">

Until now, we dealt with implicit types of namespaces, also know as
interfaces. Having explicitly declared interface enable more
expressivity and type safety. Interfaces are introduced by the keyword
`interface` and their bodies lists names of types and values paired
with their type, like so:

```jsligo group=contract2
interface FA0_INTF {
  type storage;
  @entry const add : (s : int, k : storage) => [list<operation>, storage];
}
```

An interface can then be used to constrain a namespace definition,
ensuring that said namespace contains *at least* the types and values
listed in the given interface, like so:

```jsligo group=contract2
namespace FA0 implements FA0_INTF {
  type storage = int;
  @entry const add = (s : int, k : int) : [list<operation>, int] => [list([]), s + k];
  @entry const extra = (s : int, k : int) : [list<operation>, int] => [list([]), s - k];
}
```

Interfaces can be extended by inheritance, like so:

```jsligo group=contract2
interface FABase_INTF {
  type t;
};

interface FA0_INTF extends FABase_INTF {
  @entry const transfer : (_u : unit, s : t) => [list<operation>, t];
};

interface FA0Ext_INTF extends FA0_INTF {
  @entry const transfer1 : (_u : unit, s : t) => [list<operation>, t];
};

interface FA1_INTF extends FABase_INTF {
  @entry const transfer2 : (_u : unit, s : t) => [list<operation>, t];
};
```

Note how the abstract type `t` in `FABase_INTF` remains abstract.

It is possible to design diamond inheritance, that is, inheriting
twice the same base interface, like so:

```jsligo group=contract2
interface FAAll_INTF extends FA0Ext_INTF, FA1_INTF {
  @entry const transfer3 : (_u : unit, s : t) => [list<operation>, t];
  @view const v1 : (_u : unit, s : t) => t;
  @entry const opt_val? : (i : int, s : t) => [list<operation>, t];
}
```

Here, the abstract type `t` was inherited twice from
`FABase_INTF`. Note the *optional value* `opt_val`, distinghished as
such by a question mark: `opt_val?`. This means that a namespace
implementing `FAAll_INTF` can choose not to implement
`opt_val`. The implementation of an interface can be done as follows:

```jsligo group=contract2
namespace FAAll_wo_opt_val implements FAAll_INTF {
  type t = int;

  @entry const transfer = (_u : unit, s : t) : [list<operation>, t] => [list([]), s];
  @entry const transfer1 = (_u : unit, s : t) : [list<operation>, t] => [list([]), s];
  @entry const transfer2 = (_u : unit, s : t) : [list<operation>, t] => [list([]), s];
  @entry const transfer3 = (_u : unit, s : t) : [list<operation>, t] => [list([]), s];
  @view const v1 = (_u : unit, s : t) : t => s;

  /* "foo", "transfer4" and "v2" are not in "FAAll_INTF", but can
     nevertheless be added here, because "implements" does not filter,
     but only have the compiler check that the fields in the interface
     are implemented. */

  export const foo = (s : t) : t => s;
  @entry const transfer4 = (_u : unit, s : t) : [list<operation>, t] => [list([]), s];
  @view const v2 = (_u : unit, s : t) : t => s;
}

```

</Syntax>


<!-- updated use of entry -->
