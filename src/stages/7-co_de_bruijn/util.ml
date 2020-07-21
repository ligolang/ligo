(* A "usage" says whether we use something (Keep) or not (Drop) *)
type usage = Keep | Drop

(* A list of usages describes how we use a list of things, saying
   whether we use each thing in the list or not *)
type usages = usage list

(* Accordingly, usages are interpreted as certain functions on lists *)
let rec select (us : usages) (xs : 'a list) : 'a list =
  match (us, xs) with
  | ([], []) -> []
  | (Drop :: us, _ :: xs) -> select us xs
  | (Keep :: us, x :: xs) -> x :: select us xs
  | _ -> failwith ("select with mismatched scopes " ^ __LOC__)

(* We can compose two usages.

   Spec: select (compose us1 us2) xs = select us1 (select us2 xs) *)
let rec compose (us1 : usages) (us2 : usages) : usages =
  match (us1, us2) with
  | (us1, []) -> us1
  | (us1, Drop :: us2) -> Drop :: compose us1 us2
  | (Drop :: us1, Keep :: us2) -> Drop :: compose us1 us2
  | (Keep :: us1, Keep :: us2) -> Keep :: compose us1 us2
  | _ -> failwith ("compose with mismatched scopes " ^ __LOC__)

(* Count the number of used things *)
let rec num_used = function
  | [] -> 0
  | Drop :: us -> num_used us
  | Keep :: us -> 1 + num_used us



(* A "splitting" describes two (possibly overlapping) usages of the
   same list, where everything is used either on the left or right (or
   both.) "Everybody's Got To Be Somewhere." *)
type side = Left | Right | Both
type splitting = side list

(* Accordingly, a splitting can be interpreted as two usages of the
   same length, its left and right usages. *)
let rec left_usages (s : splitting) : usages =
  match s with
  | [] -> []
  | Left :: s -> Keep :: left_usages s
  | Right :: s -> Drop :: left_usages s
  | Both :: s -> Keep :: left_usages s

let rec right_usages (s : splitting) : usages =
  match s with
  | [] -> []
  | Left :: s -> Drop :: right_usages s
  | Right :: s -> Keep :: right_usages s
  | Both :: s -> Keep :: right_usages s

let usages (s : splitting) : usages * usages =
  (left_usages s, right_usages s)

(* So, in turn, a splitting means a way to split a list into two
   parts. Nothing is lost, everybody's got to be somewhere. *)
let split (s : splitting) (xs : 'a list) : 'a list * 'a list =
  let (l, r) = usages s in (select l xs, select r xs)

let rec flip (s : splitting) : splitting =
  match s with
  | [] -> []
  | Left :: s -> Right :: flip s
  | Right :: s -> Left :: flip s
  | Both :: s -> Both :: flip s


(* Given _any_ two usages of the same list, we can produce a
   "smallest" splitting such that:

   let (s, u) = coproduct ls rs in
   compose (left_usages s) u = ls
   compose (right_usages s) u = rs

   The usages will discard the unused stuff. *)
let rec coproduct (ls : usages) (rs : usages) : splitting * usages =
  match (ls, rs) with
  | ([], []) -> ([], [])
  | (l :: ls, r :: rs) ->
    let (s, u) = coproduct ls rs in
    (match (l, r) with
     | (Drop, Drop) -> (s, Drop :: u)
     | (Drop, Keep) -> (Right :: s, Keep :: u)
     | (Keep, Drop) -> (Left :: s, Keep :: u)
     | (Keep, Keep) -> (Both :: s, Keep :: u))
   | _ -> failwith ("coproduct of mismatched scopes " ^ __LOC__)


(* Using the coproduct, given two compatible splittings `outer` (top)
   and `inner` (bottom) like so:

           Δ₂
           / \
          /   \
         |     |
         Γ   Δ₁
        / \
       /   \
      |     |
     Γ₁   Γ₂

   we can reassociate like so:

        Δ₂
        / \
       /   \
      |     |
     Γ₁    ?
           / \
          /   \
         |     |
        Γ₂   Δ₁
*)
let assoc (outer : splitting) (inner : splitting) : splitting * splitting =
  let (outerL, outerR) = usages outer in (* tau, ups *)
  let (innerL, innerR) = usages inner in (* rho, sig *)
  let (inner', outerR') = coproduct (compose innerR outerL) outerR in
  let (outer', _) = coproduct (compose innerL outerL) outerR' in
  (outer', inner')

(* To make things slightly more clear, we define a type of
   things-with-splittings. The splitting splits an implicit scope into
   the scope for the left thing and the scope for the right thing. *)
type ('a, 'b) split = splitting * 'a * 'b

(* given left usages, produce a splitting by keeping everything on the right *)
let rec rights_with (us : usages) : splitting =
  match us with
  | [] -> []
  | Drop :: us -> Right :: rights_with us
  | Keep :: us -> Both :: rights_with us


let pp_usage ppf = function
  | Keep -> Format.pp_print_string ppf "Keep"
  | Drop -> Format.pp_print_string ppf "Drop"

let pp_usages ppf us =
  Format.fprintf ppf
    "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
       pp_usage)
    us

let pp_side ppf = function
  | Left -> Format.pp_print_string ppf "Left"
  | Right -> Format.pp_print_string ppf "Right"
  | Both -> Format.pp_print_string ppf "Both"

let pp_splitting ppf us =
  Format.fprintf ppf
    "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
       pp_side)
    us

let rec all_usages (n : int) : usages list =
  if n <= 0
  then [[]]
  else
    List.concat
      (List.map
         (fun s -> [Drop :: s; Keep :: s])
         (all_usages (n - 1)))

let rec all_splittings (n : int) : splitting list =
  if n <= 0
  then [[]]
  else
    List.concat
      (List.map
         (fun s -> [Left :: s; Right :: s; Both :: s])
         (all_splittings (n - 1)))


(* Tests *)
let%test_module _ = (module struct
  let rec keeps = function
    | [] -> 0
    | Drop :: us -> keeps us
    | Keep :: us -> 1 + keeps us

  let rec range (n : int) : int list =
    if n <= 0
    then []
    else (0 :: List.map (fun n -> n+1) (range (n - 1)))

  let for_all xs p = List.for_all p xs

  (* test compose *)
  let%test _ =
    for_all (all_usages 5) @@ fun us2 ->
    for_all (all_usages (keeps us2)) @@ fun us1 ->
    let xs = range (List.length us2) in
    select (compose us1 us2) xs = select us1 (select us2 xs)

  (* test assoc *)
  let%test _ =
    for_all (all_splittings 5) @@ fun outer ->
    for_all (all_splittings (keeps (left_usages outer))) @@ fun inner ->
    let (outer', inner') = assoc outer inner in
    (left_usages outer' = compose (left_usages inner) (left_usages outer))
    && (compose (left_usages inner') (right_usages outer') = compose (right_usages inner) (left_usages outer))
    && (compose (right_usages inner') (right_usages outer') = right_usages outer)
end)
