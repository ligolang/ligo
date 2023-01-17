let escaping xs = 
  let mut i = 0 in
  let f = fun _ -> 
    let () = i := i + 1 in
    i
  in
  List.iter f xs

let main ((), storage : unit * int) : operation list * storage = 
  [], storage