---
id: functions
title: Functions
---

import Syntax from '@theme/Syntax';

Polymorphic functions accept arguments of parametric types, that is,
a larger category of inputs, as long as the function does not assume a
particular type for the argument (so called *uniform polymorphism*).

Perhaps the most trivial example is the identity function.

For any given type `t`, there is a canonical function from type `t` to
type `t`: it takes an argument and returns it immediately. For
instance, we can write the identity function for `int` as follows:

<Syntax syntax="cameligo">

```cameligo group=monomorphism
let id_int (x : int) = x
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=monomorphism
const id_int = (x: int) : int => x;
```

</Syntax>

However, if we would want to use the same function on a different
type, such as `nat`, we will need to write a new definition:

<Syntax syntax="cameligo">

```cameligo group=monomorphism
let id_nat (x : nat) = x
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=monomorphism
const id_nat = (x : nat) : nat => x;
```

</Syntax>

If we read carefully, we see that there is almost no difference
between `id_int` and `id_nat`: it is just the type that changes, but
for the rest, the body of the function remains the same.

Thanks to parametric polymorphism, we can write a single function
declaration that works for both cases.

<Syntax syntax="cameligo">

```cameligo group=polymorphism
let id (type a) (x : a) : a = x
```

Here we introduce a type variable `a` which can be generalised using
`(type a)` after the function name in the declaration. If we have more
than one type parameter, we list them like so:

```cameligo group=polymorphism
let map (type a b) (f : a -> b) (l : a list) : b list = List.map f l
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=polymorphism
const id = <T>(x: T) : T => x;
```

Here `T` is a type variable which can be generalised. If we have more
than one type parameter, we list them like so:

```jsligo group=polymorphism
const map = <A,B>(f: (x:A) => B, l: list<A>) : list<B> => List.map (f,l);
```

</Syntax>

We can now call the function `id` with arguments of different
types:

<Syntax syntax="cameligo">

```cameligo group=polymorphism
let three_int : int = id 3
let three_string : string = id "three"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=polymorphism
const three_int : int = id(3);
const three_string : string = id("three");
```

</Syntax>

During compilation, LIGO will *monomorphise* the polymorphic functions
into specific instances, resulting in Michelson code that does not
contain polymorphic function declarations anymore.
