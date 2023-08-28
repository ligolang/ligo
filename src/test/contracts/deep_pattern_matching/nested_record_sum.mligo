type a =
| A
| B of int
| C of int * int * int
| D
| E

type r =
  {
   a : int;
   b : a
  }

let f (a : r) =
  match a with
    {
   a = _;
   b = A
  } -> 1
  | {
   b = B (_);
   a = _
  } -> 2
  | {
   a = _;
   b = C (_, _, _)
  } -> 3
  | {
   b = D;
   a = _
  } -> 4
  | {
   a = _;
   b = E
  } -> 5

[@entry]
let main (_ : unit) (_ : int) : (operation list * int) =
  let x =
    f
      {
       a = 1;
       b = A
      } in
  ([], x)
