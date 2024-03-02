---
id: import
title: "#import"
---

import Syntax from '@theme/Syntax';

The `#import` directive is specific to the LIGO compiler. It provides
the support for a minimal module system.

<Syntax syntax="cameligo">

Modules get more handy when they can be made from a file, separate
from our own program, like a library: when we *import* a module from
such a file, we automatically obtain a module encapsulating all the
definitions in it. This will become very handy for organising large
contracts, as we can divide it into different files, and the module
system keeps the naming space clean (no need for name mangling).

Generally, we will take a set of definitions that can be naturally
grouped by functionality, and put them together in a separate
file. For example, we can create a file `euro.mligo`:

```cameligo group=module_imports
type t = nat

let add (a, b : t * t) : t = a + b

let one : t = 1n
let two : t = 2n
```

In another file, we can import `euro.mligo` as a module, and use its
definitions. For example, we can create a `main.mligo` that imports
all definitions from `euro.mligo` as the module `Euro`:

```cameligo group=main_importer
#import "gitlab-pages/docs/modules/src/euro.mligo" "Euro"

type storage = Euro.t

let tip (s : storage) : storage = Euro.add (s, Euro.one)
```

</Syntax>

<Syntax syntax="jsligo">

Namespaces get more handy when they can be made from a file, separate
from our own program, like a library: when we *import* a namespace
from such a file, we automatically obtain a namespace encapsulating
all the definitions in it. This will become very handy for organising
large contracts, as we can divide it into different files, and the
namespace system keeps the naming space clean (no need for name
mangling).

Generally, we will take a set of definitions that can be naturally
grouped by functionality, and put them together in a separate
file. For example, we can create a file `euro.jsligo`:

```jsligo group=namespace_imports
export type t = nat;

export const add = (a: t, b: t): t => a + b;

export const one: t = 1n;
export const two: t = 2n;
```

In another file, we can import `euro.jsligo` as a namespace, and use
its definitions. For example, we can create a `main.jsligo` that
imports all definitions from `euro.jsligo` as the namespace `Euro`:

```jsligo group=main_importer
#import "gitlab-pages/docs/modules/src/euro.jsligo" "Euro"

type storage = Euro.t;

const tip = (s : storage) : storage =>
  Euro.add (s, Euro.one);
```

</Syntax>
