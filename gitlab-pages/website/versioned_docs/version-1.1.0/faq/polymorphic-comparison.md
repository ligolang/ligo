---
id: polymorphic-comparison
title: How to write a polymorphic comparison function ?
---

import Syntax from '@theme/Syntax';

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


The problem here is that LIGO usually tries to prevent you from seeing Michelson typechecking errors, by raising errors early when you do something that might cause a Michelson typechecking error.

If LIGO allowed comparisons a = b on any types, you might get such an error -- because not all types are "comparable" in Michelson -- and it might be difficult to understand the raw Michelson error, or for LIGO to explain the error to you in terms of your source program.

Some future version of LIGO's type system could theoretically provide a safe typechecked way to do this, where the polymorphism can be restricted to comparable types. But that doesn't seem likely to happen anytime soon...

In the meantime, if you are willing to risk seeing a Michelson typechecking error, it is possible to work around this by using "unsafe" inline Michelson:

```cameligo
[@inline] let equal (type a) (val_a : a) (val_b : a) =
  [%Michelson ({|{ UNPAIR; COMPARE; EQ }|} : a * a -> bool)] (val_a, val_b)
```

If you apply this to types which aren't comparable in Michelson, you will get a Michelson typechecking error.

<!-- updated use of entry -->