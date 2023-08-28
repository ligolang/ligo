(*
  Here, we expect a warning on all assignations of [warn_me],
  because of ambiguous constructor type, each time for a different reason.
  [TopTop] is ambiguous because it's defined twice at toplevel
  [TopA]   is ambiguous because it's defined at toplevel and in module [Mod_a]
  [BA]     is ambiguous because it's defined in both [Mod_b] and [Mod_a]
  etc.

  NOTE : This test also checks the priority order of infered sum types.
  For example, matching types at toplevel should be selected
  in priority before those in modules,
  so [TopA] should be infered as [ttop] and not [ta]
  so the warning should be "inferred ttop but could be ta"
  and not "inferred ta but could be ttop"
*)

module Mod_a = struct
  type ta =
  | AA of int
  | TopA of int
  | BA of int
  | AN of int

  type ta2 = AA of int

  let dummy = 0

  module Mod_nested = struct
    type tn =
    | AN of int
    | NN of int
    | BN of int

    type tn2 = NN of int

  end

end

module Mod_b = struct
  type tb =
  | BB of int
  | TopB of int
  | BA of int
  | BN of int

  type tb2 = BB of int

  let dummy = 0

end

type ts1 = TopS1 of int

(* Shadowed by [t] below but should remain accessible *)

type ts1 = ts1 list

type ts2 = TopS2 of int

(* Shadowed and should be accessible too *)

type ts2 =
  {
   x : ts2;
   y : int
  }

(* This time, it's shadowed but not referenced by the shadower
so it should not be accessible, so there should be no warning *)

type ts3 = TopS3 of int

type ts3 = tez

type ttop =
| TopTop of int
| TopA of int
| TopB of int
| TopS1 of int
| TopS2 of int
| TopS3 of int

type ttop2 = TopTop of int

let warn_me = TopTop 42

let warn_me = TopA 42

let warn_me = TopB 42

let warn_me = BA 42

let warn_me = BB 42

let warn_me = AA 42

let warn_me = BN 42

let warn_me = AN 42

(* TODO : It should infer ta and warn about tn and not the contrary *)

let warn_me = NN 42

let warn_me = TopS1 42

let warn_me = TopS2 42

let dont_warn_me = TopS3 42

[@entry]
let main (_ : int) (_ : int) : operation list * int = ([] : operation list), 42
