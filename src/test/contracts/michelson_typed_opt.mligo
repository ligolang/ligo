type r1 =
  [@layout:comb]
  { x : nat;
    y : int }

type r2 =
  [@layout:comb]
  { x : nat;
    y : int }

type p =
| One of r1
| Two of r2

let main2 (p, _ : p * nat) : operation list * nat =
  (([] : operation list),
   (match p with
    | One r -> r.x
    | Two r -> r.x))

type r3 =
  [@layout:comb]
  { x : nat;
    z : int }

type p =
| Onee of r1
| Three of r3

let main3 (p, _ : p * nat) : operation list * nat =
  (([] : operation list),
   (match p with
    | Onee r -> r.x
    | Three r -> r.x))

type r4 =
  [@layout:comb]
  { x : nat;
    y : timestamp }

type p =
| Oneee of r1
| Four of r4

let main4 (p, _ : p * nat) : operation list * nat =
  (([] : operation list),
   (match p with
    | Oneee r -> r.x
    | Four r -> r.x))
