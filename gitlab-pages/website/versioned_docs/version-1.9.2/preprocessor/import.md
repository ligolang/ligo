---
id: import
title: "#import"
---

import Syntax from '@theme/Syntax';

The `#import` directive is specific to the LIGO compiler. It provides
the support for a minimal module/namespace system.

<Syntax syntax="cameligo">

For more information about modules, see [Modules](../syntax/modules).

Modules get more handy when they can be made from a file, separate
from our own program, like a library: when we *import* a module from
such a file, we automatically obtain a module encapsulating all the
definitions in it. This will become very handy for organising large
contracts, as we can divide it into different files, and the module
system keeps the naming space clean (no need for name mangling).

Generally, we will take a set of definitions that can be naturally
grouped by functionality, and put them together in a separate
file. For example, we can create a file `euro.mligo`:

```cameligo group=euro
type t = nat

let add (a, b : t * t) : t = a + b

let one : t = 1n
let two : t = 2n
```

In another file, we can import `euro.mligo` as a module, and use its
definitions. For example, we can create a `main.mligo` that imports
all definitions from `euro.mligo` as the module `Euro`:

```cameligo group=main_importer
#import "gitlab-pages/docs/preprocessor/src/import/euro.mligo" "Euro"

type storage = Euro.t

let tip (s : storage) : storage = Euro.add (s, Euro.one)
```

</Syntax>

<Syntax syntax="jsligo">

For more information about namespaces, see [Namespaces](../syntax/modules).

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

```jsligo group=euro
export type t = nat;

export const add = (a: t, b: t): t => a + b;

export const one: t = 1n;
export const two: t = 2n;
```

In another file, we can import `euro.jsligo` as a namespace, and use
its definitions. For example, we can create a `main.jsligo` that
imports all definitions from `euro.jsligo` as the namespace `Euro`:

```jsligo group=main_importer
#import "gitlab-pages/docs/preprocessor/src/import/euro.jsligo" "Euro"

type storage = Euro.t;

const tip = (s : storage) : storage =>
  Euro.add (s, Euro.one);
```

## Importing namespaces

When you import a file with the `#import` directive, LIGO packages the file as a namespace.
Therefore, any namespaces in the file are sub-namespaces of that namespace.

However, the namespace does not export those sub-namespaces automatically.
As a result, if you import a file that contains namespaces, those namespaces are not accessible.

To work around this limitation, add the `@public` decorator to the namespaces in the file.
For example, this file defines the Euro type as a namespace with the `@public` decorator:

```jsligo group=euro_namespace_public
// This file is gitlab-pages/docs/preprocessor/src/import/euro_namespace_public.jsligo

@public
namespace Euro {
  export type t = nat;
  export const add = (a: t, b: t) : t => a + b;
  export const one: t = 1n;
  export const two: t = 2n;
};
```

Because the namespace is public, you can access it as a sub-namespace when you import the file into another file:

```jsligo group=import_euro_public
#import "gitlab-pages/docs/preprocessor/src/import/euro_namespace_public.jsligo" "Euro_import"

type euro_balance = Euro_import.Euro.t;

const add_tip = (s: euro_balance): euro_balance =>
  Euro_import.Euro.add(s, Euro_import.Euro.one);
```

</Syntax>
