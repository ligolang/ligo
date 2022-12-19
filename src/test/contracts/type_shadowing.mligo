type a = int 
type a = bool

// [D_type, D_type] shadowing
let true_ : a = true


type a = int
let f (type a) (x : a option) : a option =
  // [D_type, T_for_all] shadowing
  x

let main_f (_, st : unit * nat option) : ((operation) list * nat option) =
  match (f st) with
  | Some x -> ([], Some x)
  | None   -> ([], None)

let g (type a) (x : a) : a * int option = 
  // [T_for_all, T_for_all] shadowing
  let foo (type a) (y : a option) : a option = 
    y
  in
  x, foo None

// Not possible to get [T_for_all, D_type] shadowing
// But possible to use [type x = ... in] to get something similar

let h (type a) (_x : a option) = 
  type a = int in
  (1 : a)
