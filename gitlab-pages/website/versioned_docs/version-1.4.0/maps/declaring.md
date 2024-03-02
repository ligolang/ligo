---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

*Maps* are a data structure which associates values of the same type to
values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

As a consequence, the predefined type `map` has two parameters: the
first is the type of the keys, and the second the type of the
associated values.

The empty map is denoted by the predefined value `Map.empty`. A
non-empty map can be built by using the function `Map.literal` which
takes a list of pairs of key and values, and returns a map containing
them as bindings, and only them.

<Syntax syntax="cameligo">

```cameligo group=maps
type word       = string
type definition = string list
type dictionary = (word, definition) map

let empty_dict : dictionary = Map.empty

let dictionary : dictionary =
  Map.literal [
    ("one", ["The number 1."; "A member of a group."]);
    ("two", ["The number 2"])]
```

The `Map.literal` predefined function builds a map from a list of
key-value pairs, `(<key>, <value>)`.  Note also the "`;`" to separate
individual map bindings. Note that `("<string value>": address)` means
that we type-cast a string into an address.

Note: See the predefined
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
type word       = string;
type definition = list<string>;
type dictionary = map<word, definition>;

const empty_dict: dictionary = Map.empty;

const dictionary : dictionary =
  Map.literal (list([
    ["one", list(["The number 1.", "A member of a group."])],
    ["two", list(["The number 2."])]]));
```

The `Map.literal` predefined function builds a map from a list of
key-value pairs, `[<key>, <value>]`.  Note also the "`,`" to separate
individual map bindings. Note that `"<string value>" as address` means
that we type-cast a string into an address.

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>

> Note: Map keys are internally sorted by increasing values, so the
> type of the keys be *comparable*, that is, they obey a total order
> (any two keys can be compared).
