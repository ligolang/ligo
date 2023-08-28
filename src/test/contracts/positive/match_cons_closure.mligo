[@entry]
let main (p : nat list) (s : unit -> unit) : operation list * (unit -> unit) =
  match p with
  | [] -> (([] : operation list), s)
  | x :: xs ->
    let f = fun (_ : unit) : unit -> failwith (x + x + List.length xs) in
    (([] : operation list), f)
