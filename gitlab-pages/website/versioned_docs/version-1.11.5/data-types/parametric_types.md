---
id: parametric_types
title: Parameteric types
---

import Syntax from '@theme/Syntax';

LIGO parametric types are types that can accept values of different types.

For example, you might want to define a type that uses a string as a key and some other type as a value.
This example defines a type named `binding` that is a pair that consists of a string and some other type.
Building on the `binding` type, it defines a type named `int_binding` that uses an integer as the value and a type named `string_binding` that uses a string as the value:

<Syntax syntax="cameligo">

```cameligo group=parametric_types
type key = string
type 'value binding = key * 'value

let int_binding : int binding = "Alice", 4
let string_binding : string binding = "Bob", "cat"
```

As in OCaml, the `'value` parameter must be prefixed with a single quote (tick) to distinguish it from an already defined type.
Also, the type parameter is placed before the type name, as in `int binding`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=parametric_types
type key = string;
type binding<value> = [key, value];

const int_binding : binding<int> = ["Alice", 4];
const string_binding : binding<string> = ["Bob", "cat"];
```

</Syntax>

## Polymorphic functions

Polymorphic functions can take advantage of parametric types to accept a wide variety of inputs instead of only a single input type.
The inputs must be organized in a similar way, known as *uniform polymorphism*.
A simple example is the identity function, which is a function that returns the value that is passed to it.
For example, the identity function for an integer parameter looks like this:

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

To create an identity function for a nat or any other type, normally you would have to write a second function that is fundamentally the same.
However, thanks to parametric polymorphism, you can write a single function that works for both cases.

<Syntax syntax="cameligo">

The following polymorphic function accepts a value of any type.
It defines a type variable named `a`, lists that type in the function definition, and uses that type variable as the type of the passed parameter and as the type of the return value:

```cameligo group=polymorphism
let id (type a) (x : a) : a = x
```

Functions can have more than one type variable in the definition, as in this example:

```cameligo group=polymorphism
let map (type a b) (f : a -> b) (l : a list) : b list = List.map f l
```

</Syntax>

<Syntax syntax="jsligo">

The following polymorphic function accepts a value of any type.
It defines a type variable named `T`, lists that type in the function definition, and uses that type variable as the type of the passed parameter and as the type of the return value:

```jsligo group=polymorphism
const id = <T>(x: T) : T => x;
```

Functions can have more than one type variable in the definition, as in this example:

```jsligo group=polymorphism
const map = <A,B>(f: (x:A) => B, l: list<A>) : list<B> => List.map (f,l);
```

</Syntax>

Now the `id` function works the same way when passed parameters of different types:

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

Polymorphism is especially useful when writing functions over
parametric types, which include built-in types like lists, sets, and
maps.

For example, this function accepts a list of a parameterized type.
It uses an internal recursive function to reverse the list:

<Syntax syntax="cameligo">

```cameligo group=poly
let rev (type a) (xs : a list) : a list =
  let rec rev (type a) ((xs, acc) : a list * a list) : a list =
    match xs with
    | [] -> acc
    | x :: xs -> rev (xs, x :: acc) in
  rev (xs, ([] : a list))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=poly
function rev <T>(xs : list<T>) : list<T> {
  const rev = <T>([xs, acc] : [list<T>, list<T>]) : list<T> =>
    match(xs) {
      when([]): acc;
      when([y,...ys]): rev([ys, [y,...acc]])
    };

  return rev([xs, ([] as list<T>)]);
};
```

</Syntax>

Because the type of element in the list is parameterized, the function works on lists of any type, as in these examples:

<Syntax syntax="cameligo">

```cameligo group=poly
let lint : int list = rev [1; 2; 3]
let lnat : nat list = rev [1n; 2n; 3n]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=poly
const lint : list<int> = rev([1, 2, 3]);
const lnat : list<nat> = rev([1n, 2n, 3n]);
```

</Syntax>

During compilation, LIGO *monomorphises* polymorphic function into specific instances, resulting in Michelson code that does not contain polymorphic function declarations.

<!-- updated use of entry -->