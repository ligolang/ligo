---
id: annot
title: annot
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

The attribute `[@annot "foo"]` sets the annotation `%foo` on the compiled Michelson code of record fields and variant constructors.

</Syntax>

<Syntax syntax="jsligo">

The decorator `@annot("foo")` sets the annotation `%foo` on the compiled Michelson code of record fields and variant constructors.

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

For example, this code assigns annotations to the fields in a record:

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
useful for interoperability, where third-party programs or contracts
expect specific Michelson annotations, even if the LIGO code might not
use those names internally.

For example, this code assigns annotations to the fields in a record:

```jsligo group=annot
type transfer =
  // @layout("comb")
  { // @annot("from")
    address_from: address;
    // @annot("to")
    address_to: address;
    value: nat }
```

</Syntax>

The resulting Michelson code has annotations on the output:

```michelson
(address %from) (address %to) (nat)
```
