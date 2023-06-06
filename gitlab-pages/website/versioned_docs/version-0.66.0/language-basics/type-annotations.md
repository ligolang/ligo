---
id: type-annotations
title: Type Annotations
---

import Syntax from '@theme/Syntax';

## Annotations

In certain cases, the type of an expression cannot be properly
inferred by the compiler. In order to help the type checker, you can
annotate an expression with its desired type. Here is an example:

<Syntax syntax="pascaligo">

```pascaligo group=d
type parameter is Back | Claim | Withdraw

type storage is
  record [
    goal     : tez;
    deadline : timestamp;
    backers  : map (address, tez);
    funded   : bool
  ]

function back (var action : unit; var store : storage) : list (operation) * storage is { // Type annotation
  if Tezos.get_now() > store.deadline then failwith ("Deadline passed.");
  case store.backers[Tezos.get_sender()] of [
    None -> store.backers[Tezos.get_sender()] := Tezos.get_amount()
  | Some (_) -> skip
  ]
} with (nil, store)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=d
type parameter = Back | Claim | Withdraw

type storage = {
  owner    : address;
  goal     : tez;
  deadline : timestamp;
  backers  : (address, tez) map;
  funded   : bool
}

let back (param, store : unit * storage) : operation list * storage = // Annotation
  if Tezos.get_now () > store.deadline then failwith "Deadline passed."
  else
    match Map.find_opt (Tezos.get_sender ()) store.backers with
      None ->
        let backers = Map.update (Tezos.get_sender ()) (Some (Tezos.get_amount ())) store.backers
        in [], {store with backers=backers}
    | Some (x) -> [], store
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=d
type parameter =
  ["Back"]
| ["Claim"]
| ["Withdraw"];

type storage = {
  owner    : address,
  goal     : tez,
  deadline : timestamp,
  backers  : map<address, tez>,
  funded   : bool
};

let back = ([param, store] : [unit, storage]) : [list<operation>, storage] => { // Annotation
  let no_op = list([]);
  if (Tezos.get_now() > store.deadline) {
    return failwith ("Deadline passed.");
  }
  else {
    return match(Map.find_opt (Tezos.get_sender(), store.backers), {
      None: () => {
        let backers = Map.update(Tezos.get_sender(), Some(Tezos.get_amount()), store.backers);
        return [no_op, {...store, backers:backers}];
      },
      Some: x => [no_op, store]
    })
  };
};
```

</Syntax>
