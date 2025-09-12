---
id: annot
title: annot
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

The attribute `[@annot "foo"]` is used to set the name `%foo` of the
Michelson equivalent of record fields or variant constructors.

</Syntax>

<Syntax syntax="jsligo">

The decorator `@annot("foo")` is used to set the name `%foo` of the
Michelson equivalent of record fields or variant constructors.

</Syntax>

By default, a variant is compiled to a comb of Michelson `or` type
constructors, and each leaf in the nested tree of `or` that
corresponds to a constructor is annotated with that name (`%foo`).

Similarly, a record is compiled to a comb of Michelson `pair` type
constructors, and each leaf in the nested tree of `pair` that
corresponds to a field is annotated with that name (`%foo`).

<Syntax syntax="cameligo">

In other words, the attribute `[@annot "foo"]` allows the Michelson
annotation for a given field or constructor to be customized. This is
useful for interoperability, where a third-party programs or contracts
expect specific Michelson annotations, even if the LIGO code might not
use those names internally.

For example,

```cameligo group=annot
type transfer =
  [@layout comb]
  { [@annot from] address_from : address;
    [@annot to] address_to : address;
    value : nat }
```

</Syntax>

<Syntax syntax="jsligo">

In other words, the decorator `@annot("foo")` allows the Michelson
annotation for a given field or constructor to be customized. This is
useful for interoperability, where a third-party programs or contracts
expect specific Michelson annotations, even if the LIGO code might not
use those names internally.

For example,

```jsligo group=annot
type transfer =
  @layout("comb")
  { @annot("from") address_from: address;
    @annot("to") address_to: address;
    value: nat }
```

</Syntax>
