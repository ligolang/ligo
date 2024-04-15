---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

Ordinary maps are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a map holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big maps*. Big maps are a data structure offered by
Michelson which handles the scaling concerns for us. In LIGO, the
interface for big maps is analogous to the one used for ordinary
maps.

For convenience, we duplicate here the documentation for maps in order
to feature big maps, and we point out any relevant differences:

  * The hash of the keys are internally used to access the entries of
    a big map, so this is advantageous when keys are large.

  * Since big maps are lazily-accessed maps, it makes no sense iterate
    over them. In particular, there is not equivalent to the function
    `Map.size`.

  * Big maps are not *packable*, as this would assume that they are
    entirely serialised first.

Big maps are a data structure which associates values of the same type
to values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

As a consequence, the predefined type `big_map` has two parameters:
the first is the type of the keys, and the second the type of the
associated values.

The empty big map is denoted by the predefined value
`Big_map.empty`. A non-empty big map can be built by using the
function `Big_map.literal` which takes a list of pairs of key and
values, and returns a big map containing them as bindings, and only
them.

<Syntax syntax="cameligo">

```cameligo group=big_maps
type word       = string
type definition = string list
type dictionary = (word, definition) big_map

let empty_dict : dictionary = Big_map.empty

let dictionary : dictionary =
  Big_map.literal [
    ("one", ["The number 1."; "A member of a group."]);
    ("two", ["The number 2"])]
```

The `Big_map.literal` predefined function builds a big map from a list
of key-value pairs, `(<key>, <value>)`.  Note also the "`;`" to
separate individual map bindings. Note that `("<string value>":
address)` means that we type-cast a string into an address.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_maps
type word       = string;
type definition = list<string>;
type dictionary = big_map<word, definition>;

const empty_dict: dictionary = Big_map.empty;

const dictionary : dictionary =
  Big_map.literal([
    ["one", (["The number 1.", "A member of a group."] as definition)],
    ["two", (["The number 2."] as definition)]]);
```

The `Big_map.literal` predefined function builds a big map from a list
of key-value pairs, `[<key>, <value>]`.  Note also the "`,`" to
separate individual big map bindings. Note that `"<string value>" as
address` means that we type-cast a string into an address.

</Syntax>

> Note: Map keys are internally sorted by increasing values, so the
> type of the keys be *comparable*, that is, they obey a total order
> (any two keys can be compared).
