---
id: option-reference
title: Option
description: Option operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
val unopt : option ('a) -> 'a
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val unopt : 'a option -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let unopt: option&lt;'a&gt; => 'a
</SyntaxTitle>

Returns the value if it is wrapped in the `Some` constructor

This function fails if the value is `None`.

<Syntax syntax="pascaligo">

```pascaligo group=option_unopt

const value_opt: option (int) = Some (1);

const value : int = Option.unopt (value_opt); (* 1 *)

const none : int = Option.unopt ((None : option (int))); (* fails with "option is None" *)

```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=option_unopt

let value_opt : int option = Some 1

let value : int = Option.unopt value_opt (* 1 *)

let none : int = Option.unopt (None : int option) (* fails with "option is None" *)

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=option_unopt

let value_opt : option<int> = Some(1);

let value : int = Option.unopt (value_opt); /* 1 */

let none : int = Option.unopt (None() as option<int>); /* fails with "option is None" */

```

</Syntax>


<SyntaxTitle syntax="pascaligo">
val unopt_with_error : option ('a) -> string -> 'a
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val unopt_with_error : 'a option -> string -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let unopt_with_error: (value : option&lt;'a&gt;, error_msg : string) => 'a
</SyntaxTitle>

Returns the value if it is wrapped in the `Some` constructor

This function fails with the provided message if the value is `None`.


<Syntax syntax="pascaligo">

```pascaligo group=option_unopt_with_error

const value_opt: option (int) = Some (1);

const value : int = Option.unopt_with_error (value_opt, "FooBar"); (* 1 *)

const none : int = Option.unopt_with_error ((None : option (int)), "FooBar"); (* fails with "FooBar" *)

```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=option_unopt_with_error

let value_opt : int option = Some 1

let value : int = Option.unopt_with_error value_opt "FooBar" (* 1 *)

let none : int = Option.unopt_with_error (None : int option) "FooBar" (* fails with "FooBar" *)

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=option_unopt_with_error

let value_opt : option<int> = Some(1);

let value : int = Option.unopt_with_error (value_opt, "FooBar"); /* 1 */

let none : int = Option.unopt_with_error (None() as option<int>, "FooBar"); /* fails with "FooBar" */

```

</Syntax>

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

<Syntax syntax="pascaligo">

```pascaligo group=option_map

const value: option (int) = Some (1);

function foo (const _ : int): string is "foo"

const foo_option : option (string) = Option.map (foo, value) (* Some ("foo") *)

const none : option (string) = Option.map (foo, (None : option (int))) (* None *)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=option_map

let value : int option = Some 1

let foo (_ : int) : string = "foo"

let foo_option : string option = Option.map foo value (* Some "foo" *)

let none : string option = Option.map foo (None : int option) (* None *)

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=option_map

let value : option<int> = Some(1);

let foo = (_ : int) : string => "foo";

let foo_option : option<string> = Option.map (foo, value); /* Some "foo" */

let none : option<string> = Option.map (foo, None() as option<int>); /* None */

```

</Syntax>
