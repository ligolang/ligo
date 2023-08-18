type r1 =
  [@layout comb]
  {
   x : nat;
   y : int
  }

type r2 =
  [@layout comb]
  {
   x : nat;
   y : int
  }

type r3 =
  [@layout comb]
  {
   x : nat;
   z : int
  }

type r4 =
  [@layout comb]
  {
   x : nat;
   y : timestamp
  }

module A = struct
  type p =
  | One of r1
  | Two of r2

  let main (p : p) (_ : nat) : operation list * nat =
    ([],
     (match p with
        One r -> r.x
      | Two r -> r.x))

end

module B = struct
  type p =
  | Onee of r1
  | Three of r3

  let main (p : p) (_ : nat) : operation list * nat =
    ([],
     (match p with
        Onee r -> r.x
      | Three r -> r.x))

end

module C = struct
  type p =
  | Oneee of r1
  | Four of r4

  let main (p : p) (_ : nat) : operation list * nat =
    ([],
     (match p with
        Oneee r -> r.x
      | Four r -> r.x))

end
