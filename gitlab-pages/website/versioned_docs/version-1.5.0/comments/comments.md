---
id: comments
title: Comments
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">
CameLIGO features the same comments as those of OCaml:

```cameligo group=comments
(* This is a block comment
   (* and one "nested" *)*)
```

The contents of comments are scanned, and therefore strings and
comments in comments, like `"nested"` and `(* and one "nested" *)`,
need to be valid (properly closed). This enables commenting out a
random piece of contract that may already contain comments. The
downside is that you can only comment code or text whose strings and
comments are valid.
</Syntax>

<Syntax syntax="jsligo">
JsLIGO features multi-line comments like JavaScript, but additionally
accepts their nesting:

```jsligo group=comments
/* This is a multi-line comment
   /* and one "nested" */*/
```

The contents of comments are scanned, and therefore strings and
comments in comments, like `"nested"` and `/* and one "nested" */`,
need to be valid (properly closed). This enables commenting out a
random piece of contract that may already contain comments. The
downside is that you can only comment code or text whose strings and
comments are valid.

</Syntax>

LIGO also offers *single line comments* à la JavaScript:

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

> Comments can contain UTF-8 glyphs. Given that the generated
> Michelson does not contain comments, UTF-8 may matter for
> documentation of the source, and error messages display line offsets
> in the source as numbers of such glyphs, not bytes.
