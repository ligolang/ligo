
(*
  Here, with the parentheses, we have a tuple nested in another
  The diff should suggest a change between the two sub-tuples.

  A supposition made in the type diff is that,
  when the tuples contain subtuples, they are probably meant to be the same.
  For example :
    int * string * (nat * tez * nat) *          tez
  vs.
    int *          (nat * tez * int) * string * tez * address
          ^^^^^^                ^^^
  Here, we suppose the probable desired diff is :
    DELETE string
    CHANGE (nat * tez * nat) TO (nat * tez * int) (TODO NP : Ideally have diff of subtuples somehow)
    ADD    string
    keep   tez
    ADD    address
  But if all changes were considered equal, we would have :
    CHANGE string            TO (nat * tez * int)
    CHANGE (nat * tez * nat) TO string
    keep   tez
    ADD    address

*)

type a =    int * string * (nat * tez * nat) *          tez
type b =    int *          (nat * tez * int) * string * tez * address

let main (_p, s : int * int) : operation list * int =
  let  x : a = 1, "a", (1n, 1tez, 1n), 1tez in
  let _y : b = x in
  ([] : operation list), s
