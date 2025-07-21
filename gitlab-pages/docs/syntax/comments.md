---
id: comments
title: Comments
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

CameLIGO multi-line comments work in a way similar to OCaml comments:

```cameligo group=comments
(*
  This is a comment.
*)
```

CameLIGO comments can be nested as long as each comment is properly closed.
Nesting comments like this allows you to comment out a large piece of code that may include other comments.

```cameligo group=comments
(* This is a multi-line comment.
  (* This is a "nested" comment. *)
*)
```

</Syntax>

<Syntax syntax="jsligo">

JsLIGO multi-line comments work in a way similar to JavaScript comments:

```jsligo group=comments
/*
  This is a multi-line comment
*/
```

</Syntax>

Both LIGO syntaxes also use single-line comments like JavaScript.
Any code between two slashes (`//`) and the end of the current line is treated as a comment.

<Syntax syntax="cameligo">

```cameligo group=comments
let x = 10 // This is a single line comment
```
</Syntax>

<Syntax syntax="jsligo">

```jsligo group=comments
const x = 10; // This is a single line comment
```
</Syntax>

:::note

Comments can contain UTF-8 glyphs.
Although comments are not included in the generated Michelson code, UTF-8 glyphs may affect error messages and other compiler messages.
Depending on how your editor handles these characters, the lines and columns in compiler messages may not line up with the lines and columns in your editor.

:::
