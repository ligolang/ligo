---
id: big-map-reference
title: Big Maps — Scalable Maps
---

import Syntax from '@theme/Syntax';

Ordinary maps are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a map holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big maps*. Big maps are a data structure offered by
Michelson which handles the scaling concerns for us. In LIGO, the
interface for big maps is analogous to the one used for ordinary maps.

# Declaring a Map


<Syntax syntax="pascaligo">

```pascaligo group=big_maps
type move is int * int
type register is big_map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
type move = int * int
type register = (address, move) big_map
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
type move = (int, int);
type register = big_map (address, move);
```

</Syntax>



# Creating an Empty Big Map



<Syntax syntax="pascaligo">

```pascaligo group=big_maps
const empty : register = big_map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
let empty : register = Big_map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
let empty : register = Big_map.empty
```

</Syntax>


# Creating a Non-empty Map



<Syntax syntax="pascaligo">

```pascaligo group=big_maps
const moves : register =
  big_map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
let moves : register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

</Syntax>


# Accessing Values


<Syntax syntax="pascaligo">

```pascaligo group=big_maps
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
let my_balance : option (move) =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```

</Syntax>



# Updating Big Maps


<Syntax syntax="pascaligo">

```pascaligo group=big_maps
function add (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = add (moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
let updated_map : register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);
```

</Syntax>


# Removing Bindings



<Syntax syntax="pascaligo">

```pascaligo group=big_maps
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
let updated_map : register =
  Map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
let updated_map : register =
  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

</Syntax>

