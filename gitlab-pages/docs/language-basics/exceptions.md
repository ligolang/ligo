---
id: exceptions
title: Exceptions
---

To interupt the flow of execution and exit the program. In case of inconsistant value or
the caller falling to authentifies. Ligo offer several mechanism.

## The failwith function

The failwith function takes a message and exit the program displaying the message


<Syntax syntax="pascaligo">

```pascaligo group=failwith
type parameter is
  Zero of nat
| Pos  of nat

type storage is unit

type return is list (operation) * storage

function main (const p : parameter; const s : storage) : return is
  block {
    case p of
      Zero (n) -> if n > 0n then failwith ("fail") else skip
    | Pos (n)  -> if n > 0n then skip else failwith ("fail")
    end
  }
  with ((nil : list (operation)), s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=failwith
type storage = unit

let main (p, store : unit * storage) : operation list * storage =
  (failwith "This contract always fails" : operation list * storage)
```

In cameligo, the call to failwith should be annoted with a type as the typechecker cannot infer the correct type yet

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=failwith
let main = (p : unit, s : unit) =>
  if (true) { failwith("This contract always fails"); };
```

</Syntax>

## The assert and assert_some functions

The assert functions familly check that a certain condition is verified and return unit or exit the program with a default message if the condition is not verified.
`assert` check that the boolean argument is `true` and `assert_some` check that the option argument is not `none`

<Syntax syntax="pascaligo">

```pascaligo group=failwith
function main (const p : bool; const s : storage) : return is
  block {
	  assert (p)
  }
  with ((nil : list (operation)), s)

function some (const o : option (unit)) is assert_some (o)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=failwith
let main (p, s : bool * unit) =
  let u : unit = assert p
  in ([] : operation list), s

let some (o : unit option) =
  assert_some o
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=failwith
let main = (p : bool, s : unit) => {
  let u : unit = assert (p);
  ([]: list (operation), s);
};

let some = (o : option (unit)) => {
  assert_some (o)
};
```

</Syntax>
