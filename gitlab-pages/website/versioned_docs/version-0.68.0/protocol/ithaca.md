---
id: ithaca
title: Ithaca
description: Ithaca changes
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

## API

### New primitives / behaviour

#### Subtraction operator `-`

The behaviour of the subtraction operator `-` on values of type `tez` has been changed in protocol Ithaca.

Subtracting values of type `tez` yeilds on optional value. (This emits
the `SUB_MUTEZ` Michelson instruction)

<Syntax syntax="pascaligo">

```pascaligo group=b

const d : option (tez) = 5mutez - 1mutez (* Some (4mutez) *)

const e : option (tez) = 1mutez - 5mutez (* None *)

```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=b
let d : tez option = 5mutez - 1mutez (* Some (4mutez) *)
let e : tez option = 1mutez - 5mutez (* None *)

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
let d : option<tez> = (5 as mutez) - (1 as mutez); /* Some (4mutez) */
let e : option<tez> = (1 as mutez) - (5 as mutez); /* None */

```

</Syntax>


#### Option Module

<SyntaxTitle syntax="pascaligo">
val map : ('a -> 'b) -> option ('a) -> option ('b)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val map : ('a -> 'b) -> 'a option -> 'b option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let map: (f : ((item: 'a) => 'b), value : option&lt;'a&gt;) => option&lt;'b&gt;
</SyntaxTitle>

Applies the mapper function to the value if it is wrapped in the `Some` constructor.

If the value is `None` the function is not executed/applied.


## Feature no longer supported


### Test

<SyntaxTitle syntax="pascaligo">
val set_now : timestamp -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val set_now : timestamp -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let set_now = (now: timestamp) => unit
</SyntaxTitle>

We no longer support `Test.set_now` in LIGO `0.38.0` onwards, this is because
the underlying functions used by the LIGO Testing framework do not support
setting exact timestamps.
