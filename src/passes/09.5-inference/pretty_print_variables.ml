module Pair = Simple_utils.Pair

let global_mutable_pending_prints : (unit -> unit) list ref = ref []

let queue_print (print_thunk : unit -> unit) = global_mutable_pending_prints := print_thunk :: !global_mutable_pending_prints

let rec number_to_letters_ : int -> string = fun n ->
  let chr = match n mod 26 with
      0 -> "a" | 1 -> "b" | 2 -> "c" | 3 -> "d" | 4 -> "e" | 5 -> "f" | 6 -> "g" | 7 -> "h" | 8 -> "i" | 9 -> "j" | 10 -> "k" | 11 -> "l" | 12 -> "m" | 13 -> "n" | 14 -> "o" | 15 -> "p" | 16 -> "q" | 17 -> "r" | 18 -> "s" | 19 -> "t" | 20 -> "u" | 21 -> "v" | 22 -> "w" | 23 -> "x" | 24 -> "y" | 25 -> "z"
    | _ -> failwith (Format.asprintf "Cannot convert number %d to sequence of letters, is it a negative numer?" n)
  in
  if n = 0 then "" else number_to_letters_ (n / 26) ^ chr

let number_to_letters : int -> string = fun n ->
  if n = 0 then "a" else number_to_letters_ n

let flush_pending_print (state : _ Solver_types.typer_state) =
  let partition = UnionFind.Poly2.partitions state.aliases in
  if false then Format.(eprintf "Partition : %a\n%!" (Ast_core.PP.list_sep_d (Ast_core.PP.list_sep_d Ast_core.Var.pp)) partition);
  global_mutable_pending_prints := []
