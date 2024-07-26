---
id: polymorphic-comparison
title: How to write a polymorphic comparison function ?
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

> I'm trying to write some functions in (came)ligo that compare several values as long as they are ints, strings, or nats. compare_equal is one of them.
>
> This errors out with Only composed types of not more than two element are allowed to be compared.
>
> ```cameligo
> let compare_equal (type k) (a, b: k * k) : bool =
>     if(a = b) then true
>     else false
> ```
>
> Is it possible to convert a and b to their composed types?
>
> ```cameligo
> let compare_equal (type k) (a, b: k * k) : bool =
>     match a with
>         int(v) -> if(a=b) then true else false
>      |  string(v) -> if(a=b) then true else false
> ```
>

</Syntax>

<Syntax syntax="jsligo">

> I'm trying to write some functions in (came)ligo that compare several values as long as they are ints, strings, or nats. compare_equal is one of them.
>
> This errors out with Only composed types of not more than two element are allowed to be compared.
>
> ```jsligo skip
> const compare_equal = <k>(a : k, b: k) : bool => {
>     if (a == b) {
>       return true;
>     } else {
>       return false;
>     }
> }
> ```
>
> Is it possible to convert a and b to their composed types?
>
> ```jsligo skip
> const compare_equal = <k>(a : k, b: k) : bool =>
>     match (a) {
>       when(int(v)): do { if (a = b) { return true; } else { return false; } };
>       when(string(v)): do { if (a = b) { return true; } else { return false; } };
>     }
> ```
>

</Syntax>

The problem here is that LIGO usually tries to prevent you from seeing Michelson typechecking errors, by raising errors early when you do something that might cause a Michelson typechecking error.

If LIGO allowed comparisons `a = b` on any types, you might get such
an error -- because not all types are "comparable" in Michelson -- and
it might be difficult to understand the raw Michelson error, or for
LIGO to explain the error to you in terms of your source program.

In the meantime, if you are willing to risk seeing a Michelson
typechecking error, it is possible to work around this by using
"unsafe" inline Michelson:

<Syntax syntax="camligo">

```cameligo
[@inline] let compare_equal (type k) (a : k) (b : k) =
  [%Michelson ({|{ UNPAIR; COMPARE; EQ }|} : k * k -> bool)] (a, b)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
// @inline
const compare_equal = <k>(a : k, b : k) : bool =>
  (Michelson `{ UNPAIR; COMPARE; EQ }` as ((x : [k, k]) => bool)) ([a, b])
```

</Syntax>

If you apply this to types which aren't comparable in Michelson, you will get a Michelson typechecking error.

<!-- updated use of entry -->
