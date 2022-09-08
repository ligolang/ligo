let iter_op (s : int list) : unit =
  let do_nothing = fun (_ : int) -> unit
  in List.iter do_nothing s

let test (s : int list) : unit = iter_op s
