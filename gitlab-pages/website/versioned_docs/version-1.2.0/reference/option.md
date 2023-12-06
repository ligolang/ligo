---
id: option-reference
title: Option
description: Option operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val unopt : 'a option -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let unopt: option&lt;'a&gt; => 'a
</SyntaxTitle>

Returns the value if it is wrapped in the `Some` constructor

This function fails if the value is `None`.

<Syntax syntax="cameligo">

```cameligo group=option_unopt

let v_opt : int option = Some 1

let v : int = Option.unopt v_opt (* 1 *)

let none : int = Option.unopt (None : int option) (* fails with "option is None" *)

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=option_unopt

let v_opt : option<int> = Some(1);

let v : int = Option.unopt (v_opt); /* 1 */

let none : int = Option.unopt (None() as option<int>); /* fails with "option is None" */

```

</Syntax>

<SyntaxTitle syntax="cameligo">
val unopt_with_error : 'a option -> string -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let unopt_with_error: (value : option&lt;'a&gt;, error_msg : string) => 'a
</SyntaxTitle>

Returns the value if it is wrapped in the `Some` constructor

This function fails with the provided message if the value is `None`.

<Syntax syntax="cameligo">

```cameligo group=option_unopt_with_error

let v_opt : int option = Some 1

let v : int = Option.unopt_with_error v_opt "FooBar" (* 1 *)

let none : int = Option.unopt_with_error (None : int option) "FooBar" (* fails with "FooBar" *)

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=option_unopt_with_error

let v_opt : option<int> = Some(1);

let v : int = Option.unopt_with_error (v_opt, "FooBar"); /* 1 */

let none : int = Option.unopt_with_error (None() as option<int>, "FooBar"); /* fails with "FooBar" */

```

</Syntax>

<SyntaxTitle syntax="cameligo">
val value : 'a -> 'a option -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let value: (default: 'a, value : option&lt;'a&gt;) => 'a
</SyntaxTitle>

Returns the value if the second argument is wrapped in the `Some` constructor, or returns the first argument if it is `None`.

<SyntaxTitle syntax="cameligo">
val value_exn : 'e -> 'a option -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let value_exn: (err: 'e, value : option&lt;'a&gt;) => 'a
</SyntaxTitle>

Returns the value if the second argument is wrapped in the `Some` constructor, or fails with the first value if it is `None`.


<SyntaxTitle syntax="cameligo">
val map : ('a -> 'b) -> 'a option -> 'b option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let map: (f : ((item: 'a) => 'b), value : option&lt;'a&gt;) => option&lt;'b&gt;
</SyntaxTitle>

Applies the mapper function to the value if it is wrapped in the `Some` constructor.

If the value is `None`, the function is not called.

<Syntax syntax="cameligo">

```cameligo group=option_map

let v : int option = Some 1

let foo (_ : int) : string = "foo"

let foo_option : string option = Option.map foo v (* Some "foo" *)

let none : string option = Option.map foo (None : int option) (* None *)

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=option_map

let v : option<int> = Some(1);

let foo = (_ : int) : string => "foo";

let foo_option : option<string> = Option.map (foo, v); /* Some "foo" */

let none : option<string> = Option.map (foo, None() as option<int>); /* None */

```

</Syntax>

<SyntaxTitle syntax="cameligo">
val is_none : 'a option -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let is_none: option&lt;'a&gt; => bool
</SyntaxTitle>

Returns a boolean signaling if the value is `None`.

<SyntaxTitle syntax="cameligo">
val is_some : 'a option -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let is_some: option&lt;'a&gt; => bool
</SyntaxTitle>

Returns a boolean signaling if the value is a `Some`.
