---
id: layout
title: layout
---

import Syntax from '@theme/Syntax'

Michelson does not natively support records or variants. These have to
be encoded using nested `pair` or `or` type constructors, therefore
many tree representations could translate to the same linear sequence
of fields or constructors, depending on the traversal.

LIGO enables to choose between a *right comb*, which preserves the
order of the fields or constructors as declared in the source code,
and a *left-balanced, alphabetically ordered binary tree*.

<Syntax syntax="cameligo">

The attributes `[@layout "comb"]` and `[@layout "tree"]` can be placed
before a record type expression (that is, before `{ ... }`), and
before the leading vertical bar `|` of a variant type expression, in
order to explicitly choose the desired layout.

For example,

```cameligo group=layout
type transfer =
  [@layout comb]
  { [@annot from] address_from : address;
    [@annot to] address_to : address;
    value : nat }
```

</Syntax>

<Syntax syntax="jsligo">

The decorators `@layout("comb")` and `@layout("tree")` can be placed
before a record type expression (that is, before `{ ... }`), and
before the leading vertical bar `|` of a variant type expression, in
order to explicitly choose the desired layout.

For example,

```jsligo group=layout
type transfer =
  @layout("comb")
  { @annot("from") address_from: address;
    @annot("to") address_to: address;
    value: nat }
```

</Syntax>

Note that the default layout is `comb`, and, except for
inter-operations with pre-1.0.0, it should not be updated.
